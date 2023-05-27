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

;; (test-result-failed-executions failed-result)
;; (test-result-shrunk-executions failed-result)

;; (assoc-execution-state failed-result)

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

(defn- print-sequence [commands stacktrace?]
  (doseq [[[handle cmd & args] trace] commands]
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
                     (if stacktrace?
                       (with-out-str
                         (.printStackTrace ^Throwable (:exception trace)
                                           (java.io.PrintWriter. *out*)))
                       (.toString ^Object (:exception trace)))
                     trace))))))

(defn print-execution
  ([{:keys [sequential parallel]} stacktrace?]
   (print-execution sequential parallel stacktrace?))
  ([sequential parallel stacktrace?]
   (printf "Sequential prefix:\n")
   (print-sequence sequential stacktrace?)
   (doseq [[i thread] (map vector (range) parallel)]
     ;; (printf "\nThread %s:\n" (g/index->letter i))
     (printf "\nThread %s:\n" i)
     (print-sequence thread stacktrace?))))

(defn render-onto [inspector coll]
  (update-in inspector [:rendered] concat coll))

(defn render [inspector & values]
  (render-onto inspector values))

(defn render-ln [inspector & values]
  (render-onto inspector (concat values '((:newline)))))

(defn render-value [inspector value]
  (let [{:keys [counter]} inspector
        expr `(:value ~(inspect/inspect-value value) ~counter)]
    (-> inspector
        (update-in [:index] conj value)
        (update-in [:counter] inc)
        (update-in [:rendered] concat (list expr)))))

(defn- inspect-argument [inspector argument]
  (-> (render inspector " ")
      (render-value argument)))

(defn- inspect-arguments [inspector arguments]
  (-> (reduce (fn [inspector argument]
                (inspect-argument inspector argument))
              inspector arguments)
      (render-ln)))

(defn- inspect-command [inspector [[handle cmd & args] trace]]
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
      (render "  ")
      (render (pr-str handle))
      (render " = ")
      (render (:name cmd))
      (inspect-arguments args)))

(defn- inspect-sequence [inspector commands]
  (reduce (fn [inspector command]
            (inspect-command inspector command))
          inspector commands))

(defn inspect-execution
  ([inspector {:keys [sequential parallel]}]
   (inspect-execution inspector sequential parallel))
  ([inspector sequential parallel]
   (-> inspector
       (render-ln "Sequential prefix:")
       (inspect-sequence sequential))
   ;; (doseq [[i thread] (map vector (range) parallel)]
   ;;   ;; (printf "\nThread %s:\n" (g/index->letter i))
   ;;   (printf "\nThread %s:\n" i)
   ;;   (inspect-sequence inspector thread stacktrace?))
   ))

(defn- new-renderer []
  {:path []
   :rendered []})

(defn render-event
  [renderer event]
  (render-ln renderer "Hello world"))

;; (render-event (new-renderer) (analyze-event failed-result))

(defn render-reports
  [reports]
  (reduce (fn [state report]
            (render-event state report))
          (new-renderer) reports))

(defn- stateful-check-render-reply
  [{:keys [ns var] :as msg}]
  (let [report (current-test-report)
        results (test-report-events report ns var)]
    (if-let [results (seq (filter test-event-failed? results))]
      {:status :done
       :reports (render-reports (map analyze-event results))}
      {:status :no-report})))

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
