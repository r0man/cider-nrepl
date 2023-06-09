(ns stateful-check.debugger.analyzer
  (:require [orchard.inspect :as inspect]
            [stateful-check.core :as stateful-check]
            [stateful-check.debugger.cursor :as cursor]
            [stateful-check.symbolic-values :as symbolic])
  (:import [java.util Base64 UUID]
           [stateful_check.runner CaughtException]))

(defn analyzer
  "Make a new analyzer."
  [& [{:keys [path render]}]]
  {:path (vec path)
   :render (or render pr-str)})

(defn push-path
  "Push the `paths` onto the current path of `analyzer`."
  [analyzer & paths]
  (update analyzer :path into paths))

(defn- path
  "Return the concatenation of the `analyzer` path and `paths`."
  [analyzer & paths]
  (into (:path analyzer []) paths))

(defn- cursor
  "Return the concatenation of the `analyzer` path and `paths` as a cursor."
  [analyzer & paths]
  (cursor/cursor (apply path analyzer paths)))

(defn- find-argument-list [command args]
  (let [arg-num (count args)]
    (first (filter #(= arg-num (count %))
                   (-> command :meta :arglists)))))

(defn- render-value [analyzer value]
  (if (satisfies? symbolic/SymbolicValue value)
    (pr-str value)
    (binding [inspect/*max-atom-length* 50]
      (inspect/inspect-value value))))

(defn- analyze-value [analyzer value]
  {:cursor (cursor analyzer :value)
   :path (path analyzer :value)
   :rendered (render-value analyzer value)
   :value value})

(defn- analyze-argument [analyzer command args index value]
  (let [arg-list (find-argument-list command args)
        arg-name (str (nth arg-list index index))]
    (cond-> (analyze-value analyzer value)
      true (assoc :index index)
      arg-name (assoc :name arg-name))))

(defn- analyze-command
  "Analyze the Stateful Check `cmd` map."
  [cmd]
  (let [meta-data (some-> cmd :command meta)]
    (cond-> {:name (:name cmd)}
      meta-data (assoc :meta meta-data))))

(defn- analyze-arguments
  "Analyze the Stateful Check `args`."
  [analyzer command args]
  (let [analyzer (push-path analyzer :arguments)]
    (mapv (fn [[index value]]
            (analyze-argument (push-path analyzer index) command args index value))
          (map-indexed vector args))))

(defn- analyze-handle
  "Analyze the command `handle`."
  [analyzer handle]
  (let [analyzer (push-path analyzer :handle)]
    (analyze-value analyzer handle)))

(defn- analyze-result
  "Analyze the command `result`."
  [analyzer result]
  (let [analyzer (push-path analyzer :result)]
    (cond-> (analyze-value analyzer result)
      (instance? CaughtException result)
      (assoc :exception {:message (ex-message (:exception result))} ))))

(defn- analyze-failure-event
  "Analyze the test report failure `event`."
  [analyzer {:keys [actual expected] :as event}]
  (cond-> event
    (contains? event :actual)
    (update :actual #(analyze-value (push-path analyzer :actual) %))
    (contains? event :expected)
    (update :expected #(analyze-value (push-path analyzer :expected) %))))

(defn- analyze-failure-events
  "Analyze the test report failure `events`."
  [analyzer events]
  (let [analyzer (push-path analyzer :events)]
    (mapv (fn [[index event]]
            (analyze-failure-event (push-path analyzer index) event))
          (map-indexed vector events))))

(defn- analyze-failure
  "Analyze the command `failure`."
  [analyzer {:keys [message events] :as failure}]
  (cond-> {}
    message
    (assoc :message message)
    (seq events)
    (assoc :events (analyze-failure-events analyzer events))))

(defn- analyze-failures
  "Analyze the command `result`."
  [analyzer failures]
  (let [analyzer (push-path analyzer :failures)]
    (mapv (fn [[index failure]]
            (analyze-failure (push-path analyzer index) failure))
          (map-indexed vector failures))))

(defn- analyze-state
  "Analyze the command `state`."
  [analyzer state]
  (let [analyzer (push-path analyzer :state)]
    (analyze-value analyzer state)))

(defn- analyze-execution-trace
  "Analyze the execution `trace`."
  [analyzer commands]
  (mapv (fn [[index [[handle cmd & args] result-str result]]]
          (let [analyzer (push-path analyzer index)
                command (analyze-command cmd)
                bindings (get-in analyzer [:environment handle :state :before])
                state (get-in analyzer [:environment handle :state :after])
                failures (get-in analyzer [:failures handle])]
            (cond-> {:arguments (analyze-arguments analyzer command args)
                     :command command
                     :handle (analyze-handle analyzer handle)
                     :result (analyze-result analyzer result)
                     :state (analyze-state analyzer state)}
              (seq failures)
              (assoc :failures (analyze-failures analyzer failures)))))
        (map-indexed vector commands)))

(defn- analyze-parallel
  "Analyze the parallel `executions`."
  [analyzer executions]
  (let [analyzer (push-path analyzer :parallel)]
    (mapv (fn [[index executions]]
            (analyze-execution-trace (push-path analyzer index) executions))
          (map-indexed vector executions))))

(defn- analyze-sequential
  "Analyze the sequential `executions`."
  [analyzer executions]
  (let [analyzer (push-path analyzer :sequential)]
    (analyze-execution-trace analyzer executions)))

(defn- analyze-executions
  "Analyze the sequential and parallel `executions` map."
  [analyzer {:keys [environment messages] :as executions}]
  (let [analyzer (assoc analyzer :environment environment :failures messages)]
    (-> executions
        (update :sequential #(analyze-sequential analyzer %))
        (update :parallel #(analyze-parallel analyzer %)))))

(defn analyze-results
  "Analyze the Stateful Check `results`."
  [analyzer {:keys [result-data shrunk pass?] :as results}]
  (when (and (not pass?) (:specification result-data))
    (assoc-in results [:result-data :executions]
              {:first-failing
               (analyze-executions
                (push-path analyzer :result-data :executions :first-failing)
                result-data)
               :smallest-failing
               (analyze-executions
                (push-path analyzer :result-data :executions :smallest-failing)
                (:result-data shrunk))})))

(defn analyze-test-event
  "Analyze the Clojure Test report `event`."
  [analyzer {:keys [ns var] :as event}]
  (when-let [results (:stateful-check event)]
    (-> (analyze-results analyzer results)
        (assoc :test {:ns ns :var var :event event}))))
