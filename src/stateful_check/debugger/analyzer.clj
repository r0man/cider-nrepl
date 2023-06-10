(ns stateful-check.debugger.analyzer
  (:require [stateful-check.command-utils :as u]
            [stateful-check.generator :as g]
            [stateful-check.symbolic-values :as sv]
            [haystack.analyzer :as analyzer])
  (:import [java.util Base64 UUID]))

(defn- find-argument-list [command num-args]
  (first (filter #(= num-args (count %)) (-> command :meta :arglists))))

(defn- analyze-argument
  [command arg-num {:keys [index] :as argument}]
  (let [arg-list (find-argument-list command arg-num)
        arg-name (str (nth arg-list index index))]
    (cond-> argument
      arg-name (assoc :name arg-name))))

(defn- analyze-command
  "Analyze the Stateful Check `cmd` map."
  [cmd]
  (let [meta-data (some-> cmd :command meta)]
    (cond-> cmd
      meta-data
      (assoc :meta meta-data))))

(defn- analyze-event
  "Analyze a test report event in a similar way to
  `cider.nrepl.middleware.test/test-report`, with the exception the
  difference that we don't have a test namespace, var and testing
  context available. Printing is done in the rendering code."
  [[index m]]
  (let [{:keys [actual expected fault] t :type} m]
    (merge (dissoc m :expected :actual)
           {:index index}
           (when (and (#{:fail :error} t) (not fault))
             {:expected expected})
           (when (#{:fail} t)
             {:actual actual})
           (when (#{:error} t)
             (let [exception actual]
               {:error exception
                :line (some-> exception .getStackTrace first .getLineNumber)})))))

(defn- analyze-failure
  "Analyze a postcondition failure."
  [{:keys [events] :as failure}]
  (cond-> failure
    (seq events)
    (update :events #(mapv analyze-event (map-indexed vector %)))))

(defn- analyze-sequential-environment
  "Return a map from a command handle to execution frame."
  [cmds-and-traces state bindings & [thread]]
  (first (reduce (fn [[env state bindings]
                      [index [[handle cmd-obj & symbolic-args] _result-str result]]]
                   (let [real-args (sv/get-real-value symbolic-args bindings)
                         next-bindings (assoc bindings handle result)
                         next-state {:real (u/make-next-state cmd-obj (:real state) real-args result)
                                     :symbolic (u/make-next-state cmd-obj (:symbolic state) symbolic-args handle)}
                         failure (u/check-postcondition cmd-obj (:real state) (:real next-state) real-args result)
                         command (analyze-command cmd-obj)
                         num-args (count symbolic-args)
                         frame (cond-> {:arguments (mapv (fn [index real symbolic]
                                                           (analyze-argument
                                                            command num-args
                                                            {:index index
                                                             :value {:real real :symbolic symbolic}}))
                                                         (range (count symbolic-args)) real-args symbolic-args)
                                        :bindings {:after next-bindings :before bindings}
                                        :command command
                                        :handle handle
                                        :index index
                                        :result result
                                        :state {:after next-state :before state}}
                                 failure (assoc :failure (analyze-failure failure))
                                 thread (assoc :thread thread))]
                     [(assoc env handle frame) next-state next-bindings]))
                 [{} state bindings] (map-indexed vector cmds-and-traces))))

(defn- analyze-environments
  "Return a map mapping from a command handle to execution environment."
  [spec cmds-and-traces]
  (let [setup-fn (:setup spec)
        setup-result (when-let [setup setup-fn]
                       (setup))
        bindings (if setup-fn
                   {g/setup-var setup-result}
                   {})
        init-state-fn (or (:initial-state spec)
                          (constantly nil))
        init-state (if (:setup spec)
                     (init-state-fn (get bindings g/setup-var))
                     (init-state-fn))
        sequential-env (analyze-sequential-environment
                        (:sequential cmds-and-traces)
                        {:real init-state :symbolic init-state}
                        bindings)
        last-env (get sequential-env (ffirst (last (:sequential cmds-and-traces))))]
    (into sequential-env
          (mapcat (fn [[thread sequential]]
                    (analyze-sequential-environment
                     sequential
                     (-> last-env :state :after)
                     (-> last-env :bindings :after)
                     thread))
                  (map-indexed vector (:parallel cmds-and-traces))))))

(defn- analyze-sequential-executions
  "Analyze the sequential executions."
  [environments executions]
  (vec (for [[[handle cmd-obj & symbolic-args] result-str] executions]
         (get environments handle))))

(defn- analyze-parallel-executions
  "Analyze the parallel executions."
  [environments executions]
  (mapv #(analyze-sequential-executions environments %) executions))

(defn- analyze-executions
  "Analyze the sequential and parallel executions."
  [environments {:keys [sequential parallel]}]
  {:sequential (analyze-sequential-executions environments sequential)
   :parallel (analyze-parallel-executions environments parallel)})

(defn- analyze-result-data
  [{:keys [specification] :as result-data}]
  (let [environments (analyze-environments specification result-data)
        executions (analyze-executions environments result-data)]
    (assoc result-data :environments environments :executions executions)))

(defn analyze-results
  "Analyze the Stateful Check `results`."
  [{:keys [id result-data pass?] :as results}]
  (let [{:keys [specification]} result-data]
    (when (and (not pass?) specification)
      (-> (assoc results :id (or id (UUID/randomUUID)))
          (update-in [:result-data] analyze-result-data)
          (update-in [:shrunk :result-data] analyze-result-data)))))

(defn analyze-test-event
  "Analyze the Clojure Test report `event`."
  [{:keys [ns var] :as event}]
  (when-let [results (:stateful-check event)]
    (some-> (analyze-results results)
            (assoc :test {:ns ns :var var :event event}))))
