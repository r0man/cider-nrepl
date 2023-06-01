(ns stateful-check.debugger.analyzer
  (:require [clojure.edn :as edn]
            [orchard.inspect :as inspect]
            [stateful-check.symbolic-values :as symbolic ])
  (:import (java.util Base64)))

(defn- make-cursor [path]
  (.encodeToString (Base64/getEncoder) (.getBytes (pr-str path) "UTF-8")))

(defn parse-cursor [cursor]
  (try (let [result (edn/read-string (String. (.decode (Base64/getDecoder) cursor) "UTF-8"))]
         (when (sequential? result)
           result))
       (catch Exception _)))

(defn- cursor [specification & paths]
  (make-cursor (into (:index specification []) paths)))

;; (defn- cursor [specification & paths]
;;   (into (:index specification []) paths))

(defn- find-argument-list [command args]
  (let [arg-num (count args)]
    (first (filter #(= arg-num (count %))
                   (-> command :meta :arglists)))))

(defn- analyze-value [value]
  (if (satisfies? symbolic/SymbolicValue value)
    (pr-str value)
    (binding [inspect/*max-atom-length* 50]
      (inspect/inspect-value value))))

(defn- analyze-argument [specification command args index value]
  (let [arg-list (find-argument-list command args)
        arg-name (str (nth arg-list index index))]
    (cond-> {:cursor (cursor specification :value)
             :index index
             :value value
             :rendered (analyze-value value)}
      arg-name (assoc :name arg-name))))

(defn- analyze-command
  "Analyze the Stateful Check `cmd` map."
  [cmd]
  (let [meta-data (some-> cmd :command meta)]
    (cond-> {:name (:name cmd)}
      meta-data (assoc :meta meta-data))))

(defn- analyze-arguments
  "Analyze the Stateful Check `args`."
  [specification command args]
  (let [specification (update specification :index conj :arguments)]
    (mapv (fn [[index value]]
            (-> (update specification :index conj index)
                (analyze-argument command args index value)))
          (map-indexed vector args))))

(defn- analyze-handle
  "Analyze the command `handle`."
  [specification handle]
  {:cursor (cursor specification)
   :name (pr-str handle)
   :value handle
   :rendered (pr-str handle)})

(defn- analyze-sequential-executions [specification executions]
  (let [setup-result (when-let [setup (:setup specification)]
                       (setup))
        initial-state (when-let [initial-state (:initial-state specification)]
                        (if (:setup specification)
                          (initial-state setup-result)
                          (initial-state)))
        final-traces (reduce (fn [traces [index [[handle cmd & args :as execution] trace]]]
                               (let [index (inc index)
                                     specification (update specification :index conj index)
                                     previous-state (-> (last traces) :state :value)
                                     result (apply (:command cmd) args)
                                     next-state (if-let [next-state (:next-state cmd)]
                                                  (next-state previous-state args result)
                                                  previous-state)
                                     command (analyze-command cmd)
                                     next-trace {:command command
                                                 :arguments (analyze-arguments specification command args)
                                                 :handle (analyze-handle specification handle)
                                                 :result {:cursor (cursor specification :result :value)
                                                          :value result
                                                          :rendered (inspect/inspect-value result)}
                                                 :trace {:cursor (cursor specification :trace :value)
                                                         :value trace
                                                         :rendered (inspect/inspect-value trace)}
                                                 :state {:cursor (cursor specification :state :value)
                                                         :value next-state
                                                         :rendered (inspect/inspect-value next-state)}}]
                                 (conj traces next-trace)))
                             [{:state {:cursor (cursor  specification 0 :state :value)
                                       :value initial-state
                                       :rendered (inspect/inspect-value initial-state)}}]
                             (map-indexed vector executions))]
    (when-let [cleanup (:cleanup specification)]
      (if (:setup specification)
        (cleanup setup-result)
        (cleanup)))
    final-traces))

(defn- analyze-parallel-execution [specification executions]
  (let [specification (update specification :index conj :parallel)]
    (mapv (fn [[index executions]]
            (-> (update specification :index conj index)
                (analyze-sequential-executions executions)))
          (map-indexed vector executions))))

(defn- analyze-sequential-execution [specification executions]
  (let [specification (update specification :index conj :sequential)]
    (analyze-sequential-executions specification executions)))

(defn- analyze-execution [specification {:keys [sequential parallel]}]
  {:sequential (analyze-sequential-execution specification sequential)
   :parallel (analyze-parallel-execution specification parallel)})

(defn- analyze-first-case
  "Analyze the first failing case of `specification` and `event`."
  [specification event]
  (-> (update specification :index conj :first)
      (analyze-execution (-> event :stateful-check :failures :first))))

(defn- analyze-smallest-case
  "Analyze the smallest failing case of `specification` and `event`."
  [specification event]
  (-> (update specification :index conj :smallest)
      (analyze-execution (-> event :stateful-check :failures :smallest))))

(defn- analyze-failures
  "Analyze the failures of `specification` and `event`."
  [specification event]
  (let [specification (update specification :index #(vec (concat % [:stateful-check :failures])))]
    (update-in event [:stateful-check :failures] merge
               {:first (analyze-first-case specification event)
                :smallest (analyze-smallest-case specification event)})))

(defn analyze-test-event
  "Analyze the Clojure Test `event`."
  [event]
  (let [specification (-> event :stateful-check :specification)
        specification (assoc specification :index [:results (:ns event) (:var event)])]
    (analyze-failures specification event)))
