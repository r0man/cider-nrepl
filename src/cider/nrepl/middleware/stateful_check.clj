(ns cider.nrepl.middleware.stateful-check
  (:refer-clojure :exclude [ns-aliases])
  (:require [cider.nrepl.middleware.inspect :as middleware.inspect]
            [cider.nrepl.middleware.test :as middleware.test]
            [cider.nrepl.middleware.util :refer [transform-value]]
            [cider.nrepl.middleware.util.error-handling :refer [with-safe-transport]]
            [orchard.inspect :as inspect]))

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

(defn enhance-report [report]
  (let [specification (:stateful-check.core/specification report)
        results (:stateful-check.core/results report)
        failed-execution (-> results :result :fail)
        shrunk-execution (-> report  :shrunk :smallest first)]
    (run-execution specification failed-execution)
    (run-execution specification shrunk-execution)))

(defn current-report
  "Return the current test report."
  []
  @middleware.test/current-report)

(defn test-results
  "Return all test results for `ns` and `var` from `report`."
  [report ns var]
  (let [ns (symbol ns), var (symbol var)]
    (get-in report [:results ns var])))

(defn failing-test-results
  "Return the failing test results for `ns` and `var` from `report`."
  [report ns var]
  (filter #(= :fail (:type %)) (test-results  report ns var)))

(defn report-specification
  "Return the Stateful Check specification from `report`."
  [report]
  (:stateful-check.core/specification report))

(defn report-failing-commands
  "Return the Stateful Check specification from `report`."
  [report]
  (some-> report :stateful-check.core/results :result ex-data))

(defn report-shrunk-commands
  "Return the Stateful Check specification from `report`."
  [report]
  (some-> report :stateful-check.core/results :shrunk :result ex-data))

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

(defn render-report
  [state reports]
  state)

(defn render-reports
  [reports]
  (reduce (fn [state report]
            (render-report state report))
          {:rendered []} reports))

(defn- stateful-check-render-reply
  [{:keys [ns var] :as msg}]
  (if-let [reports (seq (failing-test-results (current-report) ns var))]
    {:status :done
     :reports (render-reports (map enhance-report reports))}
    {:status :no-report}))

(defn handle-stateful-check [handler msg]
  (with-safe-transport handler msg
    "stateful-check-render" stateful-check-render-reply))

(comment

  (def my-spec
    (report-specification
     (first (failing-test-results
             (current-report)
             "cider.nrepl.middleware.stateful-check-java-map-test"
             "java-map-passes-sequentially"))))

  (def my-failing-trace
    (report-failing-commands
     (first (failing-test-results
             (current-report)
             "cider.nrepl.middleware.stateful-check-java-map-test"
             "java-map-passes-sequentially"))))

  (def my-shrunk-trace
    (report-shrunk-commands
     (first (failing-test-results
             (current-report)
             "cider.nrepl.middleware.stateful-check-java-map-test"
             "java-map-passes-sequentially"))))

  (inspect-report
   (inspect/fresh)
   (first (failing-test-results
           (current-report)
           "cider.nrepl.middleware.stateful-check-java-map-test"
           "java-map-passes-sequentially")))

  (-> (inspect-report
       (inspect/fresh)
       (first (failing-test-results
               (current-report)
               "cider.nrepl.middleware.stateful-check-java-map-test"
               "java-map-passes-sequentially")))
      (inspect/down 1))

  ;; (print-execution my-shrunk-trace false)
  ;; (clojure.pprint/pprint (inspect-execution (inspect/fresh) my-shrunk-trace))

  ;; {:a {:b 1}}

  (-> (inspect/fresh)
      (inspect/start [{:a {:b 1}}
                      {:c {:d 1}}])
      ;; (inspect/down 2)
      ;; (inspect/down 2)
      )

  (-> my-inspector
      (inspect/down 0))

  (-> (inspect/fresh)
      (inspect/start (first (failing-test-results
                             (current-report)
                             "cider.nrepl.middleware.stateful-check-java-map-test"
                             "java-map-passes-sequentially")))))
