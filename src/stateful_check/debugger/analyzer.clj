(ns stateful-check.debugger.analyzer
  (:require [orchard.inspect :as inspect]
            [stateful-check.core :as stateful-check]
            [stateful-check.debugger.cursor :as cursor]
            [stateful-check.symbolic-values :as symbolic])
  (:import (java.util Base64 UUID)))

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
    (analyze-value analyzer result)))

(defn- analyze-execution-trace
  "Analyze the execution `trace`."
  [analyzer commands]
  (mapv (fn [[index [[handle cmd & args] result]]]
          (let [analyzer (push-path analyzer index)
                command (analyze-command cmd)]
            {:arguments (analyze-arguments analyzer command args)
             :command command
             :handle (analyze-handle analyzer handle)
             :result (analyze-result analyzer result)}))
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
  [analyzer executions]
  (-> executions
      (update :sequential #(analyze-sequential analyzer %))
      (update :parallel #(analyze-parallel analyzer %))))

(defn analyze-quick-check
  "Analyze the Stateful Check `results`."
  [analyzer specification options {:keys [id result shrunk] :as results}]
  (when (stateful-check/failure-exception? result)
    {:id (str (or id (UUID/randomUUID)))
     :specification specification
     :options options
     :results results
     :executions {:first (analyze-executions
                          (push-path analyzer :executions :first)
                          (ex-data result))
                  :smallest (analyze-executions
                             (push-path analyzer :executions :smallest)
                             (ex-data (:result shrunk)))}}))

(defn analyze-test-report-event
  "Analyze the Clojure Test report `event`."
  [analyzer {:keys [ns var] :as event}]
  (let [specification (-> event :stateful-check :specification)
        options (-> event :stateful-check :options)
        results (-> event :stateful-check :results)
        id (str ns "/" var)]
    (when (and specification results)
      (-> (analyze-quick-check analyzer specification options results)
          (assoc :id id :ns ns :var var :test-report event)))))
