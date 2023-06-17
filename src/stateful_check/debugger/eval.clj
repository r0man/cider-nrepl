(ns stateful-check.debugger.eval)

(defn- sequential-state [executions index]
  (some-> (ffirst (nth executions index nil)) .-name))

(defn- parallel-state [executions index]
  (set (map #(.-name %) (keep #(ffirst (nth % index nil)) executions))))

(defn- last-sequential-state [sequential]
  (sequential-state sequential (dec (count sequential))))

(defn- last-parallel-state [parallel]
  (parallel-state parallel (dec (apply max (map count parallel)))))

(defn- add-sequential-executions
  [state-machine {:keys [sequential]}]
  (let [executions (vec sequential)]
    (reduce (fn [state-machine index]
              (let [current-state (sequential-state executions index)]
                (if-let [next-state (sequential-state executions (inc index))]
                  (assoc-in state-machine [current-state :evaluate] next-state)
                  state-machine)))
            state-machine (range (count executions)))))

(defn- add-parallel-executions
  [state-machine {:keys [sequential parallel]}]
  (let [executions (vec parallel)]
    (reduce (fn [state-machine index]
              (let [current-states (parallel-state executions index)
                    next-states (parallel-state executions (inc index))]
                (if (and (seq current-states) (seq next-states))
                  (assoc-in state-machine [current-states :evaluate] next-states)
                  state-machine)))
            (if (and (seq sequential) (seq parallel))
              (assoc-in state-machine [(last-sequential-state sequential) :evaluate]
                        (parallel-state parallel 0))
              state-machine)
            (range (apply max (map count executions))))))

(defn- add-start
  [state-machine {:keys [setup initial-state]} {:keys [sequential parallel]}]
  (assoc-in state-machine [nil :evaluate]
            (cond setup "setup"
                  initial-state "initial-state"
                  (seq sequential) (sequential-state sequential 0)
                  (seq parallel) (parallel-state parallel 0))))

(defn- add-setup
  [state-machine {:keys [setup initial-state]} {:keys [sequential parallel]}]
  (if (and setup (or (seq sequential) (seq parallel)))
    (assoc-in state-machine ["setup" :evaluate]
              (cond initial-state "initial-state"
                    (seq sequential) (sequential-state sequential 0)
                    (seq parallel) (parallel-state parallel 0)))
    state-machine))

(defn- add-initial-state
  [state-machine {:keys [initial-state]} {:keys [sequential parallel]}]
  (if (and initial-state (or (seq sequential) (seq parallel)))
    (assoc-in state-machine ["initial-state" :evaluate]
              (cond (seq sequential) (sequential-state sequential 0)
                    (seq parallel) (parallel-state parallel 0)))
    state-machine))

(defn- add-cleanup
  [state-machine {:keys [cleanup]}]
  (if cleanup
    (assoc-in state-machine ["cleanup" :evaluate] nil)
    state-machine))

(defn- add-end
  [state-machine {:keys [cleanup]} {:keys [sequential parallel]}]
  (let [target-state (if cleanup "cleanup" nil)]
    (cond (seq parallel)
          (assoc-in state-machine [(last-parallel-state parallel) :evaluate] target-state)
          (seq sequential)
          (assoc-in state-machine [(last-sequential-state sequential) :evaluate] target-state))))

(defn make-state-machine
  [specification {:keys [result-data]}]
  (-> {}
      (add-start specification result-data)
      (add-setup specification result-data)
      (add-initial-state specification result-data)
      (add-sequential-executions result-data)
      (add-parallel-executions result-data)
      (add-end specification result-data)
      (add-cleanup specification)))

(defn next-state
  [state-machine current-state transition]
  (get-in state-machine [current-state transition]))

;; (defn update-next-state
;;   [db event]
;;   (update db :state (partial next-state login-state-machine) event))

;; (defn handle-next-state
;;   [db [event _]]
;;   (update-next-state db event))
