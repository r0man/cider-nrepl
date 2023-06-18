(ns stateful-check.debugger.eval
  (:require [clojure.string :as str]
            [stateful-check.command-utils :as u]
            [stateful-check.debugger.state-machine :as state-machine]
            [stateful-check.generator :as g]
            [stateful-check.symbolic-values :as sv]))

(defn- get-environment [result-data handle & keys]
  (get-in result-data (concat [:environments (sv/->RootVar handle)] keys)))

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

(defn- update-environment [result-data state f & args]
  (update-in result-data [:environments (sv/->RootVar state)] #(apply f % args)))

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
                      (update-environment next-handle assoc-in [:bindings :eval] bindings)
                      (update-environment next-handle assoc-in [:state :eval] initial-state)))
                result-data (next-state-set state-machine :start))
        (update :state-machine state-machine/update-next-state :start))))

(defn- reset [result-data]
  (update result-data :state-machine state-machine/update-next-state :reset))

(defn- execute-command [result-data handle]
  (let [cmd-obj (get-environment result-data handle :command)
        arguments (sort-by :index (get-environment result-data handle :arguments))
        bindings (get-environment result-data handle :bindings :eval)
        real-args (sv/get-real-value (map (comp :symbolic :value) arguments) bindings)
        state (get-environment result-data handle :state :eval)]
    (try (let [value (apply (:command cmd-obj) real-args)
               next-state (u/make-next-state cmd-obj state real-args value)
               failure (u/check-postcondition cmd-obj state next-state real-args value)]
           (prn real-args value)
           (cond-> {:handle handle
                    :bindings (assoc bindings (sv/->RootVar handle) value)
                    :next-state next-state
                    :value value}
             failure (assoc :failure failure)))
         (catch Throwable error
           {:handle handle
            :bindings bindings
            :error error
            :state state}))))

(defn- add-result
  [result-data {:keys [bindings handle next-state] :as result}]
  (let [next-handles (next-handles result-data handle)]
    (-> (reduce (fn [result-data next-handle]
                  (-> result-data
                      (update-environment next-handle assoc-in [:bindings :eval] bindings)
                      (update-environment next-handle assoc-in [:state :eval] next-state)))
                result-data next-handles)
        (update-environment handle assoc-in [:result :eval] (select-keys result [:error :value])))))

(defn- execute-sequential-command
  [{:keys [state-machine] :as result-data}]
  (let [{:keys [error] :as eval-result} (execute-command result-data (:state state-machine))]
    (-> (add-result result-data eval-result)
        (update :state-machine state-machine/update-next-state :pass))))

(defn- execute-parallel-commands
  [{:keys [state-machine] :as result-data}]
  (let [;; eval-results (map deref (mapv #(future (execute-command result-data %))
        ;;                               (:state state-machine)))
        eval-results (mapv #(execute-command result-data %)
                           (:state state-machine))]
    (-> (reduce add-result result-data eval-results)
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
    (clojure.pprint/pprint (:state-machine next-result-data))
    next-result-data))

(defn evaluate [run case]
  (if (= "first" (some-> case name))
    (update run :result-data evaluate-failed-case)
    (update-in run [:shrunk :result-data] evaluate-failed-case)))
