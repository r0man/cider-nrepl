(ns stateful-check.debugger.state-machine)

(defn- sequential-state [executions index]
  (some-> (ffirst (nth executions index nil)) .-name))

(defn- parallel-state [executions index]
  (set (map #(.-name %) (keep #(ffirst (nth % index nil)) executions))))

(defn- last-sequential-state [sequential]
  (sequential-state sequential (dec (count sequential))))

(defn- last-parallel-state [parallel]
  (parallel-state parallel (dec (apply max (map count parallel)))))

(defn- stop-state [specification]
  ;; (if (:cleanup specification) "cleanup" "final")
  "final")

(defn- add-sequential-executions
  [state-machine {:keys [specification sequential]}]
  (let [executions (vec sequential)]
    (reduce (fn [state-machine index]
              (let [current-state (sequential-state executions index)]
                (if-let [next-state (sequential-state executions (inc index))]
                  (->  state-machine
                       (assoc-in [current-state :fail] current-state)
                       (assoc-in [current-state :pass] next-state)
                       (assoc-in [current-state :stop] (stop-state specification)))
                  state-machine)))
            state-machine (range (count executions)))))

(defn- add-parallel-executions
  [state-machine {:keys [specification sequential parallel]}]
  (if (seq parallel)
    (let [executions (vec parallel)]
      (reduce (fn [state-machine index]
                (let [current-state (parallel-state executions index)
                      next-state (parallel-state executions (inc index))]
                  (if (and (seq current-state) (seq next-state))
                    (->  state-machine
                         (assoc-in [current-state :fail] current-state)
                         (assoc-in [current-state :pass] next-state)
                         (assoc-in [current-state :stop] (stop-state specification)))
                    state-machine)))
              state-machine (range (apply max (map count executions)))))
    state-machine))

(defn connect-executions
  [state-machine {:keys [specification sequential parallel]}]
  (if (and (seq sequential) (seq parallel))
    (let [current-state (last-sequential-state sequential)
          next-state (parallel-state parallel 0)]
      (-> state-machine
          (assoc-in [current-state :fail] current-state)
          (assoc-in [current-state :pass] next-state)
          (assoc-in [current-state :stop] (stop-state specification))))
    state-machine))

(defn- add-start
  [state-machine {:keys [sequential parallel]}]
  (assoc-in state-machine ["init" :start]
            (cond (seq sequential)
                  (sequential-state sequential 0)
                  (seq parallel)
                  (parallel-state parallel 0)
                  :else "final")))

(defn- add-setup
  [state-machine {:keys [specification sequential parallel]}]
  (let [{:keys [setup initial-state]} specification]
    (if (and setup (or (seq sequential) (seq parallel)))
      (assoc-in state-machine ["setup" :pass]
                (cond initial-state "initial-state"
                      (seq sequential) (sequential-state sequential 0)
                      (seq parallel) (parallel-state parallel 0)))
      state-machine)))

(defn- add-initial-state
  [state-machine {:keys [specification sequential parallel]}]
  (let [{:keys [initial-state]} specification]
    (if (and initial-state (or (seq sequential) (seq parallel)))
      (assoc-in state-machine ["initial-state" :pass]
                (cond (seq sequential) (sequential-state sequential 0)
                      (seq parallel) (parallel-state parallel 0)))
      state-machine)))

(defn- add-cleanup
  [state-machine {:keys [specification]}]
  (if (:cleanup specification)
    (assoc-in state-machine ["cleanup" :pass] "final")
    state-machine))

(defn- add-end
  [state-machine {:keys [specification sequential parallel]}]
  (let [target-state (if (:cleanup specification) "cleanup" "init")
        target-state "final"]
    (cond (seq parallel)
          (assoc-in state-machine [(last-parallel-state parallel) :pass] target-state)
          (seq sequential)
          (assoc-in state-machine [(last-sequential-state sequential) :pass] target-state))))

(defn- add-end
  [state-machine {:keys [specification sequential parallel]}]
  (let [state-machine (assoc-in state-machine ["final" :reset] "init")
        target-state "final"]
    (cond (seq parallel)
          (assoc-in state-machine [(last-parallel-state parallel) :pass] target-state)
          (seq sequential)
          (assoc-in state-machine [(last-sequential-state sequential) :pass] target-state))))

(defn make-state-machine
  [{:keys [result-data]}]
  {:state "init"
   :definition
   (-> {}
       (add-start result-data)
       ;; (add-setup result-data)
       ;; (add-initial-state result-data)
       (add-sequential-executions result-data)
       (add-parallel-executions result-data)
       (connect-executions result-data)
       (add-end result-data)
       ;; (add-cleanup result-data)
       )})

(defn- current-transitions [state-machine]
  (get-in state-machine [:definition (:state state-machine)]))

(defn- valid-transition?
  [state-machine transition]
  (contains? (current-transitions state-machine) transition))

(defn get-next-state
  [state-machine transition]
  (let [current-state (:state state-machine)]
    (get-in state-machine [:definition current-state transition])))

(defn update-next-state
  [state-machine transition]
  (if (valid-transition? state-machine transition)
    (assoc state-machine :state (get-next-state state-machine transition))
    (throw (ex-info "Invalid transition"
                    {:state-machine state-machine
                     :transition transition
                     :transitions (vec (keys (current-transitions state-machine)))}))))
