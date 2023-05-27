(ns cider.nrepl.middleware.stateful-check
  (:refer-clojure :exclude [ns-aliases])
  (:require [cider.nrepl.middleware.inspect :as middleware.inspect]
            [cider.nrepl.middleware.test :as middleware.test]
            [cider.nrepl.middleware.util :refer [transform-value]]
            [cider.nrepl.middleware.util.error-handling :refer [with-safe-transport]]
            [orchard.inspect :as inspect]
            [clojure.spec.alpha :as spec]))

;; Test report

(defn current-test-report
  "Return the current test report."
  []
  @middleware.test/current-report)

(defn test-report-results
  "Return the results for `ns` and `var` from `report`."
  [report ns var]
  (let [ns (symbol ns), var (symbol var)]
    (get-in report [:results ns var])))

(defn test-report-result-failed?
  "Return the results for `ns` and `var` from `report`."
  [result]
  (= :fail (:type result)))

(defn failing-test-results
  "Return the failing test results for `ns` and `var` from `report`."
  [report ns var]
  (filter test-report-result-failed? (test-report-results report ns var)))

;; Test report result

(defn result-failed-executions
  "Return the Stateful Check specification from `report`."
  [result]
  (some-> result :stateful-check.core/results :result ex-data))

(defn result-shrunk-executions
  "Return the Stateful Check specification from `report`."
  [report]
  (some-> report :stateful-check.core/results :shrunk :result ex-data))

(defn result-specification
  "Return the Stateful Check specification from `report`."
  [result]
  (:stateful-check.core/specification result))

;; {:sequential [[(#<1>
;;                 {:args #function[cider.nrepl.middleware.stateful-check-java-map-test/fn--16760],
;;                  :command #function[cider.nrepl.middleware.stateful-check-java-map-test/fn--16762],
;;                  :next-state #function[cider.nrepl.middleware.stateful-check-java-map-test/fn--16765],
;;                  :name :put}
;;                 "a" 85)
;;                "nil"]
;;               [(#<2>
;;                 {:requires #function[cider.nrepl.middleware.stateful-check-java-map-test/fn--16771],
;;                  :args #function[cider.nrepl.middleware.stateful-check-java-map-test/fn--16773],
;;                  :command #function[cider.nrepl.middleware.stateful-check-java-map-test/fn--16775],
;;                  :postcondition #function[cider.nrepl.middleware.stateful-check-java-map-test/fn--16778],
;;                  :name :get}
;;                 "a")
;;                "\"boom\""]],
;;  :parallel []}

(defn run-sequential-execution [specification execution]
  (let [setup-result (when-let [setup (:setup specification)]
                       (setup))
        initial-state (when-let [initial-state (:initial-state specification)]
                        (if (:setup specification)
                          (initial-state setup-result)
                          (initial-state)))
        final-state (reduce (fn [steps [symbolic-var command & args]]
                              (prn (class symbolic-var))
                              (let [last-state (:state (last steps))
                                    command-result (apply (:command command) args)
                                    next-state (if-let [next-state (:next-state command)]
                                                 (next-state last-state args command-result)
                                                 last-state)
                                    next-step {:args (vec args)
                                               :command (:name command)
                                               :result command-result
                                               :state next-state
                                               :symbolic-var symbolic-var}]
                                (conj steps next-step)))
                            [{:state initial-state}] execution)]
    (when-let [cleanup (:cleanup specification)]
      (if (:setup specification)
        (cleanup setup-result)
        (cleanup)))
    final-state))

(defn run-parallel-execution [specification executions]
  (mapv #(run-sequential-execution specification %) executions))

(defn run-execution [specification execution]
  {:sequential (run-sequential-execution specification (:sequential execution))
   :parallel (run-parallel-execution specification (:parallel execution))})

(defn enhance-report [result]
  (let [specification (result-specification result)
        failed-executions (result-failed-executions result)
        shrunk-executions (result-shrunk-executions result)]
    (assoc result
           :stateful-check.core/failed-state (run-execution specification failed-executions)
           :stateful-check.core/shrunk-state (run-execution specification shrunk-executions))))

;; (enhance-report failed-result)
;; (result-failed-executions failed-result)

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
  {:index []
   :rendered []})

(defn render-report
  [renderer report]
  renderer)

(defn render-reports
  [reports]
  (reduce (fn [state report]
            (render-report state report))
          (new-renderer) reports))

(defn- stateful-check-render-reply
  [{:keys [ns var] :as msg}]
  (let [report (current-test-report)
        results (test-report-results report ns var)]
    (if-let [results (seq (filter test-report-result-failed? results))]
      {:status :done
       :reports (render-reports (map enhance-report results))}
      {:status :no-report})))

(defn handle-stateful-check [handler msg]
  (with-safe-transport handler msg
    "stateful-check-render" stateful-check-render-reply))

(comment

  (def failed-results
    (failing-test-results
     (current-test-report)
     "cider.nrepl.middleware.stateful-check-java-map-test"
     "java-map-passes-sequentially"))

  (def failed-result
    (first failed-results))

  (result-failed-executions failed-result)
  (result-shrunk-executions failed-result))
