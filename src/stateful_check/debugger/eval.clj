(ns stateful-check.debugger.eval
  (:require [stateful-check.command-utils :as u]
            [stateful-check.debugger.state-machine :as state-machine]
            [stateful-check.generator :as g]
            [stateful-check.runner :as r]
            [stateful-check.symbolic-values :as sv]
            [clojure.pprint :as pp])
  (:import [stateful_check.runner CaughtException]))

(defn- get-environment [result-data handle & keys]
  (get-in result-data (concat [:environments handle] keys)))

;; (defn- get-evaluation [result-data handle & keys]
;;   (get-in result-data (concat [:evaluations (sv/->RootVar handle)] keys)))

;; (defn- update-evaluation [result-data state f & args]
;;   (update-in result-data [:evaluations (sv/->RootVar state)] #(apply f % args)))

;; (defn- next-state-set
;;   [state-machine transition]
;;   (let [next-states (state-machine/get-next-state state-machine transition)]
;;     (if (set? next-states) next-states #{next-states})))

;; (defn- next-handles
;;   [{:keys [environments state-machine]} handle]
;;   (let [next-states (state-machine/get-next-state state-machine :pass)]
;;     (cond
;;       (string? next-states)
;;       #{next-states}
;;       (and (set? next-states)
;;            (nil? (get-in environments [(sv/->RootVar handle) :thread])))
;;       next-states
;;       :else
;;       (set (filter (fn [next-handle]
;;                      (= (get-in environments [(sv/->RootVar handle) :thread])
;;                         (get-in environments [(sv/->RootVar next-handle) :thread])))
;;                    next-states)))))

;; (defn- start
;;   [{:keys [specification state-machine] :as result-data}]
;;   (let [{:keys [setup initial-state]} specification
;;         setup-result (when (ifn? setup) (setup))
;;         bindings (if setup {g/setup-var setup-result} {})
;;         initial-state (when initial-state
;;                         (if setup
;;                           (initial-state setup-result)
;;                           (initial-state)))]
;;     (-> (reduce (fn [result-data next-handle]
;;                   (-> result-data
;;                       (update-evaluation next-handle assoc :bindings bindings)
;;                       (update-evaluation next-handle assoc :state initial-state)))
;;                 (assoc result-data :evaluations {})
;;                 (next-state-set state-machine :start))
;;         (update :state-machine state-machine/update-next-state :start))))

(defn- start
  [{:keys [specification] :as result-data}]
  (let [{:keys [setup initial-state]} specification
        setup-result (when (ifn? setup) (setup))
        bindings (if setup {g/setup-var setup-result} {})
        state (when initial-state
                (if setup
                  (initial-state setup-result)
                  (initial-state)))]
    (-> (assoc-in result-data [:evaluations (sv/->RootVar "init")] {:bindings bindings :state state})
        (update :state-machine state-machine/update-next-state :start))))

(defn- reset [result-data]
  (-> (dissoc result-data :evaluations)
      (update :state-machine state-machine/update-next-state :reset)))

(defn- previous-handle
  [{:keys [environments sequential parallel]} current-handle]
  (let [{:keys [index thread]} (get environments current-handle)]
    (cond
      ;; Sequential, first command
      (and (nil? thread) (zero? index))
      (sv/->RootVar "init")
      ;; Sequential, not first command
      (and (nil? thread) (pos-int? index))
      (ffirst (nth sequential (dec index)))
      ;; Parallel, first command, no sequential
      (and (nat-int? thread) (zero? index) (not (seq sequential)))
      (sv/->RootVar "init")
      ;; Parallel, first command, sequential
      (and (nat-int? thread) (zero? index) (seq sequential))
      (ffirst (last sequential))
      ;; Parallel, not first command
      (and (nat-int? thread) (pos-int? index))
      (ffirst (nth (nth parallel thread) (dec index))))))

(defn- previous-env
  [{:keys [environments] :as result-data} current-handle]
  (get environments (previous-handle result-data current-handle)))

(defn- get-bindings
  [result-data current-handle]
  (:bindings (previous-env result-data current-handle)))

(defn- get-state
  [result-data current-handle]
  (:state (previous-env result-data current-handle)))

(defn- execute-command [result-data handle]
  (prn "HANDLE: " handle (previous-env result-data (sv/->RootVar handle)))
  (let [handle (sv/->RootVar handle)
        cmd-obj (get-environment result-data handle :command)
        arguments (get-environment result-data handle :arguments)
        bindings (get-bindings result-data handle)
        real-args (sv/get-real-value (map (comp :symbolic :value) arguments) bindings)
        state (get-state result-data handle)
        result (try (apply (:command cmd-obj) real-args)
                    (catch Throwable exception
                      (r/->CaughtException exception)))
        next-state (u/make-next-state cmd-obj state real-args result)
        failure (u/check-postcondition cmd-obj state next-state real-args result)]
    (cond-> {:arguments arguments
             :bindings (assoc bindings handle result)
             :command cmd-obj
             :handle handle
             :state next-state}
      failure
      (assoc :failure failure)
      (instance? CaughtException result)
      (assoc-in [:result :error] (:exception result))
      (not (instance? CaughtException result))
      (assoc-in [:result :value] result))))

(defn- add-evaluation
  [result-data {:keys [arguments bindings command handle state result] :as frame}]
  ;; (let [next-handles (next-handles result-data handle)]
  ;;   (reduce (fn [result-data next-handle]
  ;;             (-> result-data
  ;;                 (update-evaluation next-handle assoc-in [:bindings] bindings)
  ;;                 (update-evaluation next-handle assoc-in [:state] state)))
  ;;           (-> result-data
  ;;               (update-evaluation handle assoc :arguments arguments :command command :result result)
  ;;               (update-evaluation handle assoc-in [:bindings] bindings)
  ;;               (update-evaluation handle assoc-in [:state] state))
  ;;           next-handles))
  (assoc-in result-data [:evaluations handle] frame))

;; (defn- execute-sequential-command
;;   [{:keys [state-machine] :as result-data}]
;;   (let [evaluation (execute-command result-data (:state state-machine))]
;;     (-> (add-evaluation result-data evaluation)
;;         (update :state-machine state-machine/update-next-state :pass))))

;; (defn- execute-parallel-commands
;;   [{:keys [state-machine] :as result-data}]
;;   (let [evaluations (map deref (mapv #(future (execute-command result-data %))
;;                                      (:state state-machine)))]
;;     (-> (reduce add-evaluation result-data evaluations)
;;         (update :state-machine state-machine/update-next-state :pass))))

(defn- execute-commands
  [{:keys [state-machine] :as result-data}]
  ;; TODO: Run in future
  (let [evaluations (mapv #(execute-command result-data %)
                          (:state state-machine))]
    (-> (reduce add-evaluation result-data evaluations)
        (update :state-machine state-machine/update-next-state :pass))))

(defn- evaluate-failed-case [result-data]
  (let [current-state (-> result-data :state-machine :state)
        next-result-data (cond (= #{"init"} current-state)
                               (start result-data)
                               (= #{"final"} current-state)
                               (reset result-data)
                               (set? current-state)
                               (execute-commands result-data))]
    (def my-data result-data)
    ;; (clojure.pprint/pprint (:state-machine next-result-data))
    ;; (clojure.pprint/pprint (:evaluations next-result-data))
    next-result-data))

(defn evaluate [run case]
  (if (= "first" (some-> case name))
    (update run :result-data evaluate-failed-case)
    (update-in run [:shrunk :result-data] evaluate-failed-case)))
