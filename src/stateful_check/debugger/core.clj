(ns stateful-check.debugger.core
  (:refer-clojure :exclude [ns-aliases])
  (:require [clojure.edn :as edn]
            [orchard.inspect :as inspect])
  (:import (java.util Base64)))

(def ^:private thread-names
  "abcdefghijklmnopqrstuvwxzy")

(defn- make-cursor [path]
  (.encodeToString (Base64/getEncoder) (.getBytes (pr-str path) "UTF-8")))

(defn- parse-cursor [cursor]
  (try (let [result (edn/read-string (String. (.decode (Base64/getDecoder) cursor) "UTF-8"))]
         (when (sequential? result)
           result))
       (catch Exception _)))

(defn- cursor [specification & paths]
  (make-cursor (into (:index specification []) paths)))

;; (defn- cursor [specification & paths]
;;   (into (:index specification []) paths))

(defn- index->letter [n]
  (nth thread-names n))

(defn- vector-inc-last [v]
  (conj (pop v) (inc (peek v))))

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

(defn- caught-exception?
  "Return true if `x` is a Stateful Check caught exception, otherwise false."
  [x]
  (= "stateful_check.runner.CaughtException" (some-> x .getClass .getName)))

(defn- symbolic-root-var? [x]
  (= "stateful_check.symbolic_values.RootVar" (some-> x .getClass .getName)))

(defn- symbolic-lookup-var? [x]
  (= "stateful_check.symbolic_values.LookupVar" (some-> x .getClass .getName)))

(defn- render-value [value]
  (cond (symbolic-root-var? value)
        (pr-str value)
        (symbolic-lookup-var? value)
        (pr-str value)
        :else (binding [inspect/*max-atom-length* 50]
                (inspect/inspect-value value))))

(defn- analyze-argument [specification command args index value]
  (let [arg-list (find-argument-list command args)
        arg-name (str (nth arg-list index index))]
    (cond-> {:cursor (cursor specification :value)
             :index index
             :value value
             :rendered (render-value value)}
      arg-name (assoc :name arg-name))))

(defn- analyze-command
  "Analyze the Stateful Check `cmd` map."
  [cmd]
  (let [meta-data (some-> cmd :command meta)]
    (cond-> {:name (:name cmd)}
      meta-data (assoc :meta meta-data))))

(defn- analyze-arguments
  "Analyze the Stateful Check `args`."
  [specification command args]
  (let [specification (update specification :index conj :arguments)]
    (mapv (fn [[index value]]
            (-> (update specification :index conj index)
                (analyze-argument command args index value)))
          (map-indexed vector args))))

(defn- analyze-handle
  "Analyze the command `handle`."
  [specification handle]
  {:cursor (cursor specification)
   :name (pr-str handle)
   :value handle
   :rendered (pr-str handle)})

(defn- analyze-sequential-executions [specification executions]
  (let [setup-result (when-let [setup (:setup specification)]
                       (setup))
        initial-state (when-let [initial-state (:initial-state specification)]
                        (if (:setup specification)
                          (initial-state setup-result)
                          (initial-state)))
        final-traces (reduce (fn [traces [index [[handle cmd & args :as execution] trace]]]
                               (let [index (inc index)
                                     specification (update specification :index conj index)
                                     previous-state (-> (last traces) :state :value)
                                     result (apply (:command cmd) args)
                                     next-state (if-let [next-state (:next-state cmd)]
                                                  (next-state previous-state args result)
                                                  previous-state)
                                     command (analyze-command cmd)
                                     next-trace {:command command
                                                 :arguments (analyze-arguments specification command args)
                                                 :handle (analyze-handle specification handle)
                                                 :result {:cursor (cursor specification :result :value)
                                                          :value result
                                                          :rendered (inspect/inspect-value result)}
                                                 :trace {:cursor (cursor specification :trace :value)
                                                         :value trace
                                                         :rendered (inspect/inspect-value trace)}
                                                 :state {:cursor (cursor specification :state :value)
                                                         :value next-state
                                                         :rendered (inspect/inspect-value next-state)}}]
                                 (conj traces next-trace)))
                             [{:state {:cursor (cursor  specification 0 :state :value)
                                       :value initial-state
                                       :rendered (inspect/inspect-value initial-state)}}]
                             (map-indexed vector executions))]
    (when-let [cleanup (:cleanup specification)]
      (if (:setup specification)
        (cleanup setup-result)
        (cleanup)))
    final-traces))

(defn- analyze-parallel-execution [specification executions]
  (let [specification (update specification :index conj :parallel)]
    (mapv (fn [[index executions]]
            (-> (update specification :index conj index)
                (analyze-sequential-executions executions)))
          (map-indexed vector executions))))

;; (def my-inspector
;;   (stateful-check-report
;;    (stateful-check-analyze (make-inspector) (test-report))
;;    {:ns 'cider.nrepl.middleware.test-stateful-check
;;     :var 'java-map-passes-sequentially}))

;; (get-in my-inspector
;;         '[:results
;;             cider.nrepl.middleware.test-stateful-check
;;             java-map-passes-sequentially
;;             :stateful-check
;;             :failures
;;             :first
;;             :parallel
;;             0
;;             0
;;             :state
;;             :rendered])

(defn- analyze-sequential-execution [specification executions]
  (let [specification (update specification :index conj :sequential)]
    (analyze-sequential-executions specification executions)))

(defn- analyze-execution [specification {:keys [sequential parallel]}]
  {:sequential (analyze-sequential-execution specification sequential)
   :parallel (analyze-parallel-execution specification parallel)})

(defn- analyze-first-case
  "Analyze the first failing case of `specification` and `event`."
  [specification event]
  (-> (update specification :index conj :first)
      (analyze-execution (test-event-failed-execution event))))

(defn- analyze-smallest-case
  "Analyze the smallest failing case of `specification` and `event`."
  [specification event]
  (-> (update specification :index conj :smallest)
      (analyze-execution (test-event-smallest-execution event))))

(defn- analyze-failures
  "Analyze the failures of `specification` and `event`."
  [specification event]
  (let [specification (update specification :index #(vec (concat % [:stateful-check :failures])))]
    (update-in event [:stateful-check :failures] merge
               {:INDEX (:index specification)
                :first (analyze-first-case specification event)
                :smallest (analyze-smallest-case specification event)})))

(defn analyze-event
  "Analyze the Clojure Test `event`."
  [event]
  (let [specification (test-event-specification event)
        specification (assoc specification :index [:results (:ns event) (:var event)])]
    (analyze-failures specification event)))

(defn get-object [inspector cursor]
  (some->> cursor parse-cursor (get-in inspector)))

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

(defn make-inspector
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

(defn filter-reports
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
  (filter-reports inspector criteria))

(comment

  (stateful-check-report
   (stateful-check-analyze (make-inspector) @cider.nrepl.middleware.test/current-report)
   {:ns 'cider.nrepl.middleware.test-stateful-check
    :var 'java-map-passes-sequentially})

  )
