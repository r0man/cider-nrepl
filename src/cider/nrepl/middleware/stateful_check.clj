(ns cider.nrepl.middleware.stateful-check
  (:refer-clojure :exclude [ns-aliases])
  (:require [cider.nrepl.middleware.inspect :as middleware.inspect]
            [cider.nrepl.middleware.test :as middleware.test]
            [cider.nrepl.middleware.util :refer [transform-value]]
            [cider.nrepl.middleware.util.error-handling :refer [with-safe-transport]]
            [orchard.inspect :as inspect]))

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
  (some-> event :stateful-check.core/results :result ex-data))

(defn- test-event-shrunk-execution
  "Return the shrunk execution from `event`."
  [event]
  (some-> event :stateful-check.core/results :shrunk :result ex-data))

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
           (run-execution specification (test-event-shrunk-execution event)))))

;; Render

(defn- render-argument [inspector argument]
  (-> (inspect/render inspector " ")
      (inspect/render-value argument)))

(defn- render-arguments [inspector arguments]
  (-> (reduce (fn [inspector argument]
                (render-argument inspector argument))
              inspector arguments)
      (inspect/render-ln)))

(defn- render-command [inspector [[handle cmd & args] trace]]
  (printf "  %s = %s %s\n"
          (pr-str handle)
          (cons (:name cmd)
                args)
          (if (= :stateful-check.runner/unevaluated trace)
            ""
            (str " = "
                 (if ;; (instance? stateful_check.runner.CaughtException trace)
                     (= "stateful_check.runner.CaughtException"
                        (.getName (.getClass trace)))
                   ;; (if stacktrace?
                   ;;   (with-out-str
                   ;;     (.printStackTrace ^Throwable (:exception trace)
                   ;;                       (java.io.PrintWriter. *out*)))
                   ;;   (.toString ^Object (:exception trace)))
                   trace))))
  (-> inspector
      (inspect/render "  ")
      (inspect/render (pr-str handle))
      (inspect/render " = ")
      (inspect/render (:name cmd))
      (render-arguments args)))

(defn- render-command-sequence [inspector commands]
  (reduce (fn [inspector command]
            (render-command inspector command))
          inspector commands))

(defn render-execution
  ([inspector {:keys [sequential parallel]}]
   (render-execution inspector sequential parallel))
  ([inspector sequential parallel]
   (-> inspector
       (inspect/render-ln "Sequential prefix:")
       (render-command-sequence sequential))
   ;; (doseq [[i thread] (map vector (range) parallel)]
   ;;   ;; (printf "\nThread %s:\n" (g/index->letter i))
   ;;   (printf "\nThread %s:\n" i)
   ;;   (inspect-sequence inspector thread stacktrace?))
   ))

(defn render-failed-execution [inspector event]
  (let [execution (test-event-failed-execution event)]
    (-> inspector
        (inspect/render-ln "--- Failed execution:")
        (inspect/render-ln)
        (render-execution execution)
        (inspect/render-ln))))

(defn render-shrunk-execution [inspector event]
  (let [execution (test-event-shrunk-execution event)]
    (-> inspector
        (inspect/render-ln "--- Shrunk execution:")
        (inspect/render-ln)
        (render-execution execution)
        (inspect/render-ln))))

(defn render-event [inspector event]
  (-> inspector
      (render-shrunk-execution event)
      (render-failed-execution event)))

(defn render-events [inspector events]
  (reduce render-event inspector events))

(defn render-report [ns var]
  (let [report (current-test-report)
        events (test-report-events report ns var)]
    (if-let [failed-events (seq (filter test-event-failed? events))]
      (-> (inspect/fresh)
          (inspect/clear)
          (inspect/render-ln "Stateful Check Test Inspector")
          (inspect/render-ln "=============================")
          (inspect/render-ln)
          (inspect/render-ln "--- Test:")
          (inspect/render-ln)
          (inspect/render-ln "  Namespace:   " (str ns))
          (inspect/render-ln "  Var:         " (str var))
          (inspect/render-ln)
          (render-events failed-events)))))

(defn- stateful-check-render-reply
  [{:keys [ns var] :as msg}]
  (let [inspector (render-report ns var)]
    (#'middleware.inspect/inspector-response
     msg (middleware.inspect/swap-inspector! msg (constantly inspector)))))

(defn handle-stateful-check [handler msg]
  (with-safe-transport handler msg
    "stateful-check-render" stateful-check-render-reply))

(comment

  (inspect/fresh)

  (def failed-results
    (filter test-event-failed?
            (test-report-events (current-test-report)
                                "cider.nrepl.middleware.stateful-check-java-map-test"
                                "java-map-passes-sequentially")))

  (def failed-result
    (first failed-results))

  (test-event-failed-execution failed-result)
  (test-event-shrunk-execution failed-result))
