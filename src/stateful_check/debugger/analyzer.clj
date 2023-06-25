(ns stateful-check.debugger.analyzer
  (:require [stateful-check.command-utils :as u]
            [stateful-check.debugger.state-machine :as state-machine]
            [stateful-check.generator :as g]
            [stateful-check.symbolic-values :as sv])
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

(defn- analyze-result
  "Analyze the execution `result` and `result-str`."
  [options result result-str]
  (let [assume-immutable-results? (-> options :run :assume-immutable-results true?)
        matches (when (string? result-str)
                  (re-matches mutated-rexeg result-str))]
    (cond-> {}
      (instance? CaughtException result)
      (assoc :error (:exception result))
      (not (instance? CaughtException result))
      (assoc :value result :value-str result-str)
      (not assume-immutable-results?)
      (assoc :mutated? (some? (seq matches)))
      ;; TODO: Would be nice to get mutation info as data, like :message, :mutated-into object
      (and (not assume-immutable-results?) (seq matches))
      (assoc :value-str (nth matches 1) :value (nth matches 3)))))

(defn- analyze-sequential-environment
  "Return a map from a command handle to execution frame."
  [{:keys [failures options]} cmds-and-traces state bindings & [thread]]
  (first (reduce (fn [[env state bindings]
                      [index [[handle cmd-obj & symbolic-args] result-str result]]]
                   (let [real-args (sv/get-real-value symbolic-args (:real bindings))
                         next-bindings {:real (assoc (:real bindings) handle result)
                                        :symbolic (assoc (:symbolic bindings) handle handle)}
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
                                                             :real real
                                                             :symbolic symbolic}))
                                                         (range (count symbolic-args)) real-args symbolic-args)
                                        :bindings next-bindings
                                        :command command
                                        :handle handle
                                        :index index
                                        :result {:real (analyze-result options result result-str)
                                                 :symbolic handle}
                                        :state next-state}
                                 (seq failures)
                                 (assoc :failures (analyze-failures failures))
                                 (nat-int? thread)
                                 (assoc :thread thread)
                                 ;; (nth cmds-and-traces (inc index) nil)
                                 ;; (assoc-in [:environment :next] (ffirst (nth cmds-and-traces (inc index))))
                                 ;; (nth cmds-and-traces (dec index) nil)
                                 ;; (assoc-in [:environment :previous] (ffirst (nth cmds-and-traces (dec index))))
                                 )]
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
                        {:real bindings :symbolic {g/setup-var g/setup-var}})
        last-env (get sequential-env (ffirst (last sequential)))
        environments (into sequential-env
                           (mapcat (fn [[thread sequential]]
                                     (analyze-sequential-environment
                                      result-data sequential
                                      (:state last-env)
                                      (:bindings last-env)
                                      thread))
                                   (map-indexed vector parallel)))]
    ;; TODO: Don't call setup (see above) and also not cleanup
    (when-let [cleanup (:cleanup specification)]
      (if (:setup specification)
        (cleanup setup-result)
        (cleanup)))
    (assoc environments (sv/->RootVar "init")
           {:bindings bindings
            :handle (sv/->RootVar "init")
            :state init-state})))

(defn- analyze-result-data
  [result-data]
  (let [environments (analyze-environments result-data)
        state-machine (state-machine/make-state-machine result-data)]
    (assoc result-data :environments environments :state-machine state-machine)))

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
