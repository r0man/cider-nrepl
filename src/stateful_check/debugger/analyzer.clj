(ns stateful-check.debugger.analyzer
  (:require [stateful-check.command-utils :as u]
            [stateful-check.generator :as g]
            [stateful-check.symbolic-values :as sv]
            [haystack.analyzer :as analyzer]
            [stateful-check.debugger.state-machine :as state-machine])
  (:import [java.util Base64 UUID]
           [stateful_check.runner CaughtException]))

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
  [[index {:keys [events] :as failure}]]
  (cond-> (assoc failure :index index)
    (seq events)
    (update :events #(mapv analyze-event (map-indexed vector %)))))

(defn- analyze-failures
  "Analyze all postcondition failures."
  [failures]
  (mapv analyze-failure (map-indexed vector failures)))

(def ^:private mutated-rexeg
  #"(?s)(.*)(\n\s+>> object may have been mutated later into (.+) <<\n)")

(defn- analyze-result
  "Analyze the execution `result` and `result-str`."
  [options result result-str]
  (let [assume-immutable-results? (-> options :run :assume-immutable-results true?)
        matches (when (string? result-str)
                  (re-matches mutated-rexeg result-str))]
    (cond-> {:value result :string-value result-str}
      (instance? CaughtException result)
      (assoc :error (:exception result))
      (instance? CaughtException result)
      (dissoc :value)
      (not assume-immutable-results?)
      ;; TODO: Can't detect this ourselves since a message has been appended to
      ;; result-str by stateful-check.
      (assoc :mutated (some? (seq matches)))
      ;; TODO: Would be nice to get mutation info as data, like :message, :mutated-into object
      (and (not assume-immutable-results?) (seq matches))
      (assoc :string-value (nth matches 1) :value (nth matches 3)))))

(defn- analyze-sequential-environment
  "Return a map from a command handle to execution frame."
  [{:keys [failures options]} cmds-and-traces state bindings & [thread]]
  (first (reduce (fn [[env state bindings]
                      [index [[handle cmd-obj & symbolic-args] result-str result]]]
                   (let [real-args (sv/get-real-value symbolic-args bindings)
                         next-bindings (assoc bindings handle result)
                         next-state {:real (u/make-next-state cmd-obj (:real state) real-args result)
                                     :symbolic (u/make-next-state cmd-obj (:symbolic state) symbolic-args handle)}
                         ;; TODO: Can we do this here? The failures captured by
                         ;; stateful-check are from all sequential and parallel
                         ;; inter-leavings. The failures captured here only from
                         ;; this failing one.
                         ;; failure (u/check-postcondition cmd-obj (:real state) (:real next-state) real-args result)
                         failures (get failures handle)
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
                                        :result (analyze-result options result result-str)
                                        :state {:after next-state :before state}}
                                 (seq failures)
                                 (assoc :failures (analyze-failures failures))
                                 thread
                                 (assoc :thread thread))]
                     [(assoc env handle frame) next-state next-bindings]))
                 [{} state bindings] (map-indexed vector cmds-and-traces))))

(defn- analyze-environments
  "Return a map mapping from a command handle to execution environment."
  [{:keys [specification sequential parallel] :as result-data}]
  (let [setup-fn (:setup specification)
        ;; TODO: Don't call setup, since it might change mutable objects. Get
        ;; initial bindings from somewhere else.
        setup-result (when-let [setup setup-fn]
                       (setup))
        bindings (if setup-fn
                   {g/setup-var setup-result}
                   {})
        init-state-fn (or (:initial-state specification)
                          (constantly nil))
        init-state (if (:setup specification)
                     (init-state-fn (get bindings g/setup-var))
                     (init-state-fn))
        sequential-env (analyze-sequential-environment
                        result-data sequential
                        {:real init-state :symbolic init-state}
                        bindings)
        last-env (get sequential-env (ffirst (last sequential)))
        environments (into sequential-env
                           (mapcat (fn [[thread sequential]]
                                     (analyze-sequential-environment
                                      result-data sequential
                                      (-> last-env :state :after)
                                      (-> last-env :bindings :after)
                                      thread))
                                   (map-indexed vector parallel)))]
    ;; TODO: Don't call setup (see above) and also not cleanup
    (when-let [cleanup (:cleanup specification)]
      (if (:setup specification)
        (cleanup setup-result)
        (cleanup)))
    environments))

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
  [result-data]
  (let [environments (analyze-environments result-data)
        executions (analyze-executions environments result-data)
        state-machine (state-machine/make-state-machine {:result-data result-data})]
    (assoc result-data
           :environments environments
           :executions executions
           :state-machine state-machine)))

(defn analyze-results
  "Analyze the Stateful Check `results`."
  [{:keys [id result-data pass?] :as results}]
  (let [{:keys [specification]} result-data]
    (cond-> (assoc results :id (str (or id (UUID/randomUUID))))
      (and specification (not pass?))
      (update-in [:result-data] analyze-result-data)
      (and specification (not pass?))
      (update-in [:shrunk :result-data] analyze-result-data))))

(defn analyze-test-event
  "Analyze the Clojure Test report `event`."
  [{:keys [ns var] :as event}]
  (when-let [results (:stateful-check event)]
    (some-> (analyze-results results)
            (update-in [:specification] assoc
                       :ns ns
                       :var var
                       :id (str ns "/" var)
                       :type :test
                       :test event))))
