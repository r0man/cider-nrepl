(ns stateful-check.debugger.eval
  (:require [stateful-check.debugger.state-machine :as state-machine]))

(defn- init [{:keys [state-machine] :as result-data}]
  (println "INIT")
  (update result-data :state-machine state-machine/update-next-state :start))

(defn- final [{:keys [state-machine] :as result-data}]
  (println "FINAL")
  (assoc-in result-data [:state-machine :state] "init"))

(defn- cleanup [{:keys [state-machine] :as result-data}]
  (println "CLEANUP")
  (update result-data :state-machine state-machine/update-next-state :pass))

(defn- setup [{:keys [state-machine] :as result-data}]
  (println "SETUP")
  (update result-data :state-machine state-machine/update-next-state :pass))

(defn- init-state [{:keys [state-machine] :as result-data}]
  (println "INIT STATE")
  (update result-data :state-machine state-machine/update-next-state :pass))

(defn- execute-sequential-command [{:keys [state-machine] :as result-data}]
  (println "SEQUENTIAL COMMAND")
  (update result-data :state-machine state-machine/update-next-state :pass))

(defn- execute-parallel-commands [{:keys [state-machine] :as result-data}]
  (println "PARALLEL COMMAND")
  (update result-data :state-machine state-machine/update-next-state :pass))

(defn- evaluate-failed-case [result-data]
  (let [current-state (-> result-data :state-machine :state)]
    (cond (= "init" current-state)
          (init result-data)
          (= "setup" current-state)
          (setup result-data)
          (= "cleanup" current-state)
          (cleanup result-data)
          (= "final" current-state)
          (final result-data)
          (string? current-state)
          (execute-sequential-command result-data)
          (set? current-state)
          (execute-parallel-commands result-data))))

(defn evaluate [run case]
  (if (= "first" (some-> case name))
    (update run :result-data evaluate-failed-case)
    (update-in run [:shrunk :result-data] evaluate-failed-case)))
