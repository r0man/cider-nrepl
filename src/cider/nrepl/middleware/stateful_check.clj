(ns cider.nrepl.middleware.stateful-check
  (:refer-clojure :exclude [ns-aliases])
  (:require [cider.nrepl.middleware.inspect :as middleware.inspect]
            [cider.nrepl.middleware.test :as middleware.test]
            [cider.nrepl.middleware.util :refer [transform-value]]
            [cider.nrepl.middleware.util.error-handling :refer [with-safe-transport]]
            [clojure.edn :as edn]
            [orchard.inspect :as inspect])
  (:import (java.util Base64)))

(def ^:private thread-names
  "abcdefghijklmnopqrstuvwxzy")

(defn- index->letter [n]
  (nth thread-names n))

(defn- vector-inc-last [v]
  (conj (pop v) (inc (peek v))))

;; Test report

(defn- test-report
  "Return the current test report."
  []
  @middleware.test/current-report)

;; Test report event

(defn- test-event-failed?
  "Return true if the test `result` has failed, otherwise false."
  [event]
  (and (:stateful-check event)
       (= :fail (:type event))))

(defn- test-event-failed-execution
  "Return the failed execution from `event`."
  [event]
  (-> event :stateful-check :failures :first))

(defn- test-event-smallest-execution
  "Return the shrunk execution from `event`."
  [event]
  (-> event :stateful-check :failures :smallest))

(defn- test-event-specification
  "Return the Stateful Check specification from `event`."
  [event]
  (-> event :stateful-check :specification))

;; Analyze

(defn- find-argument-list [command args]
  (let [arg-num (count args)]
    (first (filter #(= arg-num (count %))
                   (-> command :meta :arglists)))))

(defn- analyze-argument [command args index value]
  (let [arg-list (find-argument-list command args)
        arg-name (str (nth arg-list index index))]
    (cond-> {:index index
             :value value}
      arg-name (assoc :name arg-name))))

(defn- analyze-command
  "Analyze the Stateful Check `cmd` map."
  [cmd]
  (let [meta-data (some-> cmd :command meta)]
    (cond-> {:name (:name cmd)}
      meta-data (assoc :meta meta-data))))

(defn- analyze-arguments
  "Analyze the Stateful Check `args`."
  [command args]
  (mapv (fn [[index value]]
          (analyze-argument command args index value))
        (map-indexed vector args)))

(defn- analyze-handle
  "Analyze the command `handle`."
  [handle]
  {:name (pr-str handle)
   :value handle})

(defn- analyze-sequential-executions [specification executions]
  (let [setup-result (when-let [setup (:setup specification)]
                       (setup))
        initial-state (when-let [initial-state (:initial-state specification)]
                        (if (:setup specification)
                          (initial-state setup-result)
                          (initial-state)))
        final-traces (reduce (fn [traces [[handle cmd & args :as execution] trace]]
                               (let [previous-state (:state (last traces))
                                     result (apply (:command cmd) args)
                                     next-state (if-let [next-state (:next-state cmd)]
                                                  (next-state previous-state args result)
                                                  previous-state)
                                     command (analyze-command cmd)
                                     next-trace {:command command
                                                 :arguments (analyze-arguments command args)
                                                 :handle (analyze-handle handle)
                                                 :result result
                                                 :trace trace
                                                 :state next-state}]
                                 (conj traces next-trace)))
                             [{:state initial-state}] executions)]
    (when-let [cleanup (:cleanup specification)]
      (if (:setup specification)
        (cleanup setup-result)
        (cleanup)))
    final-traces))

(defn- analyze-parallel-execution [specification executions]
  (mapv #(analyze-sequential-executions specification %) executions))

(defn- analyze-execution [specification {:keys [sequential parallel]}]
  {:sequential (analyze-sequential-executions specification sequential)
   :parallel (analyze-parallel-execution specification parallel)})

(defn analyze-event
  "Analyze the Clojure Test `event`."
  [event]
  (let [specification (test-event-specification event)]
    (update-in event [:stateful-check :failures] merge
               {:first (analyze-execution specification (test-event-failed-execution event))
                :smallest (analyze-execution specification (test-event-smallest-execution event))})))

;; Render

(defn- make-cursor [path]
  (.encodeToString (Base64/getEncoder) (.getBytes (pr-str path) "UTF-8")))

(defn- parse-cursor [cursor]
  (try (let [result (edn/read-string (String. (.decode (Base64/getDecoder) cursor) "UTF-8"))]
         (when (sequential? result) result))
       (catch Exception _)))

(defn render-value [inspector value]
  (let [{:keys [report-path]} inspector
        expr `(:value ~(inspect/inspect-value value)
                      ~(make-cursor report-path)
                      ;; ~report-path
                      ;; ~(list :GET-IN (get-in (current-test-report) report-path))
                      )]
    (-> inspector
        (update-in [:rendered] concat (list expr)))))

(defn- render-argument [inspector argument]
  (-> (inspect/render inspector "      ")
      (render-value argument)
      (inspect/render-ln)))

(defn- render-arguments [inspector arguments]
  (-> (reduce (fn [inspector argument]
                (-> (update inspector :report-path vector-inc-last)
                    (render-argument argument)))
              (inspect/render-ln inspector "    Arguments:")
              arguments)))

(defn- caught-exception?
  "Return true if `x` is a Stateful Check caught exception, otherwise false."
  [x]
  (= "stateful_check.runner.CaughtException"
     (some-> x .getClass .getName)))

(defn get-object [report cursor]
  (some->> cursor parse-cursor (get-in report)))

(defn test-report-events [test-report]
  (->> test-report :results vals
       (mapcat vals)
       (apply concat)
       (filter test-event-failed?)))

(defn- matches-criteria? [event filters]
  (let [ns (some-> filters :ns symbol)
        var (some-> filters :var symbol)]
    (and (or (nil? ns) (= ns (:ns event)))
         (or (nil? var) (= var (:var event))))))

(defn- search-events [criteria events]
  (filter #(matches-criteria? % criteria) events))

(defn- make-inspector
  "Make a new Stateful Check Inspector."
  []
  {:summary {} :results {}})

(defn- inspector-reports
  "Return the Stateful Check reports of the `inspector`."
  [inspector]
  (->> inspector :results vals (mapcat vals)))

(defn- inspector-add-reports [inspector reports]
  (reduce (fn [inspector report]
            (assoc-in inspector [:results (:ns report) (:var report)] report))
          inspector reports))

(defn- inspector-filter
  "Return a new inspector with results filtered by `criteria`"
  [inspector criteria]
  (inspector-add-reports (assoc inspector :results {})
                         (filter #(matches-criteria? % criteria)
                                 (inspector-reports inspector))))

(defn stateful-check-analyze
  "Analyze the Stateful Check events in a Cider test report."
  [inspector test-report & [opts]]
  (reduce (fn [inspector {:keys [ns var] :as event}]
            (assoc-in inspector [:results ns var] (analyze-event event)))
          inspector (search-events opts (test-report-events test-report))))

(defn stateful-check-report
  "Return the Stateful Check reports matching `criteria`."
  [inspector & [criteria]]
  (inspector-filter inspector criteria))

;; Middleware

(defn- criteria
  "Make the search criteria map from the NREPL msg."
  [{:keys [ns var]}]
  (cond-> {}
    (or (string? ns) (symbol? ns))
    (assoc :ns (symbol (name ns)))
    (or (string? var) (symbol? var))
    (assoc :ns (symbol (name ns)))))

(defn- inspector
  "Return the Stateful Check inspector from the `msg`."
  [msg]
  (or (-> msg :session meta ::inspector)
      (make-inspector)))

(defn swap-inspector!
  "Apply `f` with `args` to the inspector of the NREPL `session`."
  [{:keys [session]} f & args]
  (-> session
      (alter-meta! update ::inspector #(apply f % args))
      (get ::inspector)))

(defn- stateful-check-analyze-reply
  "Handle a Stateful Check test analysis NREPL operation."
  [msg]
  {:stateful-check-analyze
   (-> (swap-inspector! msg #(-> (stateful-check-analyze % (test-report) (criteria msg))
                                 (inspector-filter criteria)))
       (transform-value))})

(defn- stateful-check-inspect-reply
  [{:keys [index] :as msg}]
  (if-let [object (get-object (test-report) index)]
    (let [inspector (inspect/start (inspect/fresh) object)]
      (#'middleware.inspect/inspector-response
       msg (middleware.inspect/swap-inspector! msg (constantly inspector))))
    {:status :object-not-found :index index}))

(defn- stateful-check-report-reply
  "Handle a Stateful Check test report NREPL operation."
  [msg]
  (let [inspector (stateful-check-report (inspector msg) msg)]
    {:stateful-check-report (transform-value inspector)}))

(defn handle-stateful-check
  "Handle a Stateful Check NREPL `msg`."
  [handler msg]
  (with-safe-transport handler msg
    "stateful-check-analyze" stateful-check-analyze-reply
    "stateful-check-inspect" stateful-check-inspect-reply
    "stateful-check-report" stateful-check-report-reply))

(comment

  (stateful-check-report
   (stateful-check-analyze (make-inspector) (test-report))
   {:ns 'cider.nrepl.middleware.test-stateful-check
    :var 'java-map-passes-sequentially})

  )
