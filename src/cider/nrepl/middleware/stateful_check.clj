(ns cider.nrepl.middleware.stateful-check
  (:refer-clojure :exclude [ns-aliases])
  (:require [cider.nrepl.middleware.inspect :as middleware.inspect]
            [cider.nrepl.middleware.test :as middleware.test]
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

(defn- current-test-report
  "Return the current test report."
  []
  @middleware.test/current-report)

(defn- test-report-events
  "Return the test report events for `ns` and `var` from `report`."
  [report ns var]
  (let [ns (symbol ns), var (symbol var)]
    (get-in report [:results ns var])))

;; Test report event

(defn- test-event-failed?
  "Return true if the test `result` has failed, otherwise false."
  [event]
  (= :fail (:type event)))

(defn- test-event-failed-execution
  "Return the failed execution from `event`."
  [event]
  (:stateful-check.core/failed event))

(defn- test-event-smallest-execution
  "Return the shrunk execution from `event`."
  [event]
  (:stateful-check.core/smallest event))

(defn- test-event-specification
  "Return the Stateful Check specification from `event`."
  [event]
  (:stateful-check.core/spec event))

;; Test execution

(defn- run-sequential-executions [specification executions]
  (let [setup-result (when-let [setup (:setup specification)]
                       (setup))
        initial-state (when-let [initial-state (:initial-state specification)]
                        (if (:setup specification)
                          (initial-state setup-result)
                          (initial-state)))
        final-state (reduce (fn [steps [[handle cmd & args :as execution] trace]]
                              (let [previous-state (:state (last steps))
                                    result (apply (:command cmd) args)
                                    next-state (if-let [next-state (:next-state cmd)]
                                                 (next-state previous-state args result)
                                                 previous-state)
                                    next-step {:execution {:args (vec args)
                                                           :cmd cmd
                                                           :handle handle
                                                           :result result
                                                           :trace trace}
                                               :state next-state}]
                                (conj steps next-step)))
                            [{:state initial-state}] executions)]
    (when-let [cleanup (:cleanup specification)]
      (if (:setup specification)
        (cleanup setup-result)
        (cleanup)))
    final-state))

(defn- run-parallel-execution [specification executions]
  (mapv #(run-sequential-executions specification %) executions))

(defn- run-execution [specification execution]
  {:sequential (run-sequential-executions specification (:sequential execution))
   :parallel (run-parallel-execution specification (:parallel execution))})

(defn analyze-event [event]
  (let [specification (test-event-specification event)]
    (assoc event
           :stateful-check.core/failed-execution-state
           (run-execution specification (test-event-failed-execution event))
           :stateful-check.core/shrunk-execution-state
           (run-execution specification (test-event-smallest-execution event)))))

;; Render

(defn- make-cursor [path]
  (.encodeToString (Base64/getEncoder) (.getBytes (pr-str path) "UTF-8")))

(defn- parse-cursor [cursor]
  (try (edn/read-string (String. (.decode (Base64/getDecoder) cursor) "UTF-8"))
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

(defn- render-trace [inspector trace]
  (-> inspector
      (inspect/render-ln "    Result:")
      (inspect/render "      ")
      (render-value
       (if (caught-exception? trace)
         (:exception trace)
         trace))))

(defn- render-command [inspector [[handle cmd & args] trace]]
  (printf "  %s = %s %s\n"
          (pr-str handle)
          (cons (:name cmd) args)
          (if (= :stateful-check.runner/unevaluated trace)
            ""
            (str " = "
                 (if (caught-exception? trace)
                   (if false ;; stacktrace?
                     (with-out-str
                       (.printStackTrace ^Throwable (:exception trace)
                                         (java.io.PrintWriter. *out*)))
                     (.toString ^Object (:exception trace)))
                   trace))))

  (let [{:keys [report-path]} inspector]
    (prn (get-in (current-test-report) report-path))
    (-> (update inspector :report-path conj 0)
        (inspect/render "  ")
        (inspect/render (pr-str handle))
        (inspect/render " ")
        (inspect/render-ln (:name cmd))
        (update :report-path conj 1)
        (render-arguments args)
        (update :report-path pop)
        (update :report-path vector-inc-last)
        (render-trace trace)
        (inspect/render-ln)
        (inspect/render-ln)
        (update :report-path pop))))

(defn- render-command-sequence [inspector commands]
  (reduce (fn [inspector [index command]]
            (-> (update inspector :report-path conj index)
                (render-command command)
                (update :report-path pop)))
          inspector (map-indexed vector  commands)))

(defn render-sequential-execution
  [inspector {:keys [sequential]}]
  (if-not (seq sequential)
    inspector
    (-> (update inspector :report-path conj :sequential)
        (inspect/render-ln "Sequential prefix:")
        (render-command-sequence sequential)
        (update :report-path pop))))

(defn render-parallel-threads
  [inspector commands]
  (reduce (fn [inspector [index commands]]
            (-> (update inspector :report-path conj index)
                (inspect/render-ln (format "Thread %s:" (index->letter index)))
                (render-command-sequence commands)
                (update :report-path pop)))
          inspector (map-indexed vector commands)))

(defn render-parallel-execution
  [inspector {:keys [parallel]}]
  (if-not (seq parallel)
    inspector
    (-> (update inspector :report-path conj :parallel)
        (render-parallel-threads parallel)
        (update :report-path pop))))

(defn render-execution
  [inspector execution]
  (-> inspector
      (render-sequential-execution execution)
      (render-parallel-execution execution)))

(defn render-failed-execution [inspector event]
  (let [execution (test-event-failed-execution event)]
    (-> (update inspector :report-path conj :stateful-check.core/failed)
        (inspect/render-ln "--- First failed execution:")
        (inspect/render-ln)
        (render-execution execution)
        (inspect/render-ln)
        (update :report-path pop))))

(defn render-smallest-execution [inspector event]
  (let [execution (test-event-smallest-execution event)]
    (-> (update inspector :report-path conj :stateful-check.core/smallest)
        (inspect/render-ln "--- Shrunk execution:")
        (inspect/render-ln)
        (render-execution execution)
        (inspect/render-ln)
        (update :report-path pop))))

(defn render-event [inspector event]
  (-> (update inspector :report-path conj (:index event))
      (render-smallest-execution event)
      (render-failed-execution event)))

(defn render-events [inspector events]
  (reduce render-event inspector events))

(defn render-report [ns var]
  (let [ns (symbol ns)
        var (symbol var)
        report (current-test-report)
        events (test-report-events report ns var)]
    (if-let [failed-events (seq (filter test-event-failed? events))]
      (-> {:ns ns
           :var var
           :report-path [:results ns var]
           :rendered []}
          (render-events failed-events)))))

(defn get-object [report cursor]
  (get-in report (parse-cursor cursor)))

(defn- stateful-check-inspect-reply
  [{:keys [index] :as msg}]
  (if-let [object (get-object (current-test-report) index)]
    (let [inspector (inspect/start (inspect/fresh) object)]
      (#'middleware.inspect/inspector-response
       msg (middleware.inspect/swap-inspector! msg (constantly inspector))))
    {:status :object-not-found :index index}))

(defn- stateful-check-render-reply
  [{:keys [ns var] :as msg}]
  (let [inspector (render-report ns var)]
    (#'middleware.inspect/inspector-response
     msg (middleware.inspect/swap-inspector! msg (constantly inspector)))))

(defn handle-stateful-check [handler msg]
  (with-safe-transport handler msg
    "stateful-check-render" stateful-check-render-reply
    "stateful-check-inspect" stateful-check-inspect-reply))

(comment

  (inspect/fresh)

  (def my-report
    (current-test-report))

  (test-report-events
   (current-test-report)
   'cider.nrepl.middleware.stateful-check-java-map-test
   'java-map-passes-sequentially)

  (render-report 'cider.nrepl.middleware.stateful-check-java-map-test
                 'java-map-passes-sequentially)

  (->> (render-report 'cider.nrepl.middleware.stateful-check-java-map-test
                      'java-map-passes-sequentially)
       :rendered
       (filter #(and (sequential? %)
                     (= :value (first %))))
       (map #(some-> (nth % 2 nil) parse-cursor))
        (map #(get-in (current-test-report) %)))

  (get-in (current-test-report)
          [:results 'cider.nrepl.middleware.stateful-check-java-map-test
           'java-map-passes-sequentially 0 :stateful-check.core/smallest :sequential 0])

  (def failed-results
    (filter test-event-failed?
            (test-report-events (current-test-report)
                                "cider.nrepl.middleware.stateful-check-java-map-test"
                                "java-map-passes-sequentially")))

  (def failed-result
    (first failed-results))

  (test-event-failed-execution failed-result)
  (test-event-smallest-execution failed-result))
