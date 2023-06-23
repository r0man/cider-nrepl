(ns stateful-check.debugger.eval
  (:require [stateful-check.command-utils :as u]
            [stateful-check.debugger.state-machine :as state-machine]
            [stateful-check.generator :as g]
            [stateful-check.runner :as r]
            [stateful-check.symbolic-values :as sv]
            [clojure.pprint :as pp])
  (:import [stateful_check.runner CaughtException]))

(defn- get-environment [result-data handle & keys]
  (get-in result-data (concat [:environments (sv/->RootVar handle)] keys)))

(defn- get-evaluation [result-data handle & keys]
  (get-in result-data (concat [:evaluations (sv/->RootVar handle)] keys)))

(defn- update-evaluation [result-data state f & args]
  (update-in result-data [:evaluations (sv/->RootVar state)] #(apply f % args)))

(defn- next-state-set
  [state-machine transition]
  (let [next-states (state-machine/get-next-state state-machine transition)]
    (if (set? next-states) next-states #{next-states})))

(defn- next-handles
  [{:keys [environments state-machine]} handle]
  (let [next-states (state-machine/get-next-state state-machine :pass)]
    (cond
      (string? next-states)
      #{next-states}
      (and (set? next-states)
           (nil? (get-in environments [(sv/->RootVar handle) :thread])))
      next-states
      :else
      (set (filter (fn [next-handle]
                     (= (get-in environments [(sv/->RootVar handle) :thread])
                        (get-in environments [(sv/->RootVar next-handle) :thread])))
                   next-states)))))

(defn- start
  [{:keys [specification state-machine] :as result-data}]
  (let [{:keys [setup initial-state]} specification
        setup-result (when (ifn? setup) (setup))
        bindings (if setup {g/setup-var setup-result} {})
        initial-state (when initial-state
                        (if setup
                          (initial-state setup-result)
                          (initial-state)))]
    (-> (reduce (fn [result-data next-handle]
                  (-> result-data
                      (update-evaluation next-handle assoc :bindings {:before bindings})
                      (update-evaluation next-handle assoc :state {:before initial-state})))
                (assoc result-data :evaluations {})
                (next-state-set state-machine :start))
        (update :state-machine state-machine/update-next-state :start))))

(defn- reset [result-data]
  (-> (dissoc result-data :evaluations)
      (update :state-machine state-machine/update-next-state :reset)))

(defn- execute-command [result-data handle]
  (let [cmd-obj (get-environment result-data handle :command)
        arguments (sort-by :index (get-environment result-data handle :arguments))
        bindings (get-evaluation result-data handle :bindings :before)
        real-args (sv/get-real-value (map (comp :symbolic :value) arguments) bindings)
        state (get-evaluation result-data handle :state :before)
        result (try (apply (:command cmd-obj) real-args)
                    (catch Throwable exception
                      (r/->CaughtException exception)))
        next-state (u/make-next-state cmd-obj state real-args result)
        failure (u/check-postcondition cmd-obj state next-state real-args result)]
    (cond-> {:arguments arguments
             :bindings {:before bindings :after (assoc bindings (sv/->RootVar handle) result)}
             :command cmd-obj
             :handle handle
             :state {:before state :after next-state}
             :result {:value result}}
      failure
      (assoc :failure failure)
      (instance? CaughtException result)
      (assoc-in [:result :error] (:exception result)))))

(defn- add-evaluation
  [result-data {:keys [arguments bindings command handle state result]}]
  (let [next-handles (next-handles result-data handle)]
    (reduce (fn [result-data next-handle]
              (-> result-data
                  (update-evaluation next-handle assoc-in [:bindings :before] (:after bindings))
                  (update-evaluation next-handle assoc-in [:state :before] (:after state))))
            (-> result-data
                (update-evaluation handle assoc :arguments arguments :command command :result result)
                (update-evaluation handle assoc-in [:bindings :before] (:before bindings))
                (update-evaluation handle assoc-in [:bindings :after] (:after bindings))
                (update-evaluation handle assoc-in [:state :before] (:before state))
                (update-evaluation handle assoc-in [:state :after] (:after state)))
            next-handles)))

(defn- execute-sequential-command
  [{:keys [state-machine] :as result-data}]
  (let [evaluation (execute-command result-data (:state state-machine))]
    (-> (add-evaluation result-data evaluation)
        (update :state-machine state-machine/update-next-state :pass))))

(defn- execute-parallel-commands
  [{:keys [state-machine] :as result-data}]
  (let [evaluations (map deref (mapv #(future (execute-command result-data %))
                                     (:state state-machine)))]
    (-> (reduce add-evaluation result-data evaluations)
        (update :state-machine state-machine/update-next-state :pass))))

(defn- evaluate-failed-case [result-data]
  (let [current-state (-> result-data :state-machine :state)
        next-result-data (cond (= "init" current-state)
                               (start result-data)
                               (= "final" current-state)
                               (reset result-data)
                               (string? current-state)
                               (execute-sequential-command result-data)
                               (set? current-state)
                               (execute-parallel-commands result-data))]
    ;; (def my-data next-result-data)
    ;; (clojure.pprint/pprint (:state-machine next-result-data))
    ;; (clojure.pprint/pprint (:evaluations next-result-data))
    next-result-data))

(defn evaluate [run case]
  (if (= "first" (some-> case name))
    (update run :result-data evaluate-failed-case)
    (update-in run [:shrunk :result-data] evaluate-failed-case)))
