(ns stateful-check.debugger.render
  (:require [cider.nrepl.middleware.test.extensions :refer [diffs-result]]
            [clojure.pprint :as pp]
            [orchard.inspect :as inspect]
            [stateful-check.symbolic-values :as sv]))

(defn- render-value
  "Render the given `value` as a string."
  [value]
  (cond
    (satisfies? sv/SymbolicValue value)
    (pr-str value)
    (instance? clojure.lang.Atom value)
    (pr-str value) ;; Atom's aren't printed nicely by orchard
    :else
    (binding [inspect/*max-atom-length* 50]
      (inspect/inspect-value value))))

(defn- render-result
  "Render the command execution `result`."
  [result]
  (cond-> result
    (:error result)
    (update :error str)
    (not (:mutated result))
    (update :value render-value)))

(defn- render-argument
  "Render the command execution `argument`."
  [argument]
  (-> (select-keys argument [:index :value])
      (update-in [:value :real] render-value)
      (update-in [:value :symbolic] pr-str)))

(defn- print-object
  "Print `object` using println for matcher-combinators results and pprint
   otherwise. The matcher-combinators library uses a custom print-method
   which doesn't get picked up by pprint since it uses a different dispatch
   mechanism."
  [object]
  (let [matcher-combinators-result? (= (:type (meta object))
                                       :matcher-combinators.clj-test/mismatch)
        print-fn (if matcher-combinators-result?
                   println
                   pp/pprint)]
    (with-out-str (print-fn object))))

(defn- render-event [event]
  (cond-> event
    (contains? event :actual)
    (update :actual print-object)
    (contains? event :diffs)
    (update :diffs diffs-result)
    (contains? event :expected)
    (update :expected print-object)))

(defn- render-failure [failure]
  (cond-> (select-keys failure [:events :index :message])
    (seq (:events failure))
    (update :events #(mapv render-event %))))

(defn- render-bindings [bindings]
  (into {} (for [[handle value] bindings]
             [(pr-str handle) (render-value value)])))

(defn- render-frame [frame]
  (-> (select-keys frame [:arguments :bindings :command :failures :handle :index :result :state :thread])
      (update :arguments #(mapv render-argument %))
      (update :command select-keys [:name])
      (update :failures #(mapv render-failure %))
      (update :handle pr-str)
      (update :result render-result)
      (update-in [:bindings :after] render-bindings)
      (update-in [:bindings :before] render-bindings)
      (update-in [:state :after :real] render-value)
      (update-in [:state :after :symbolic] render-value)
      (update-in [:state :before :real] render-value)
      (update-in [:state :before :symbolic] render-value)))

(defn- analyze-sequential-executions
  "Analyze the sequential executions."
  [environments executions]
  (vec (for [[[handle cmd-obj & symbolic-args] result-str] executions]
         (render-frame (get environments handle)))))

(defn- analyze-parallel-executions
  "Analyze the parallel executions."
  [environments executions]
  (mapv #(analyze-sequential-executions environments %) executions))

(defn- analyze-executions
  "Analyze the sequential and parallel executions."
  [{:keys [environments sequential parallel]}]
  {:sequential (analyze-sequential-executions environments sequential)
   :parallel (analyze-parallel-executions environments parallel)})

(defn render-result-data
  [result-data]
  (-> (select-keys result-data [:specification :options :state-machine])
      (assoc :executions (analyze-executions result-data))))

(defn- render-quickcheck-results [results]
  (-> (select-keys results [:failed-after-ms :failing-size :num-tests :seed :shrunk :result-data :pass? :time-elapsed-ms])
      (update :shrunk select-keys [:depth :result-data :time-shrinking-ms :total-nodes-visited])))

(defn render-analysis [analysis]
  (-> (render-quickcheck-results analysis)
      (merge (select-keys analysis [:id :frequencies :specification :options]))
      (update-in [:result-data] render-result-data)
      (update-in [:shrunk :result-data] render-result-data)))

(defn- render-analyses
  [analyses]
  (into {} (for [[id analysis] analyses]
             [id (render-analysis analysis)])))

(defn render-debugger
  "Render the `debugger` in a Bencode compatible format."
  [debugger]
  (update debugger :analyses render-analyses))
