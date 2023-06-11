(ns stateful-check.debugger.render
  (:require [cider.nrepl.middleware.test.extensions :refer [diffs-result]]
            [clojure.pprint :as pp]
            [orchard.inspect :as inspect]
            [stateful-check.symbolic-values :as sv]))

(defn- render-value [value]
  (if (satisfies? sv/SymbolicValue value)
    (pr-str value)
    (binding [inspect/*max-atom-length* 50]
      (inspect/inspect-value value))))

(defn- render-result [result]
  (cond-> result
    (not (:mutated result))
    (update :value render-value)))

(defn- render-argument [argument]
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

(defn- render-executions [{:keys [sequential parallel]}]
  {:sequential (mapv render-frame sequential)
   :parallel (mapv #(mapv render-frame %) parallel)})

(defn render-result-data
  [result-data]
  (-> (select-keys result-data [:executions :specification :options])
      (update :executions render-executions)))

(defn- render-quickcheck-results [results]
  (-> (select-keys results [:id :failing-size :frequencies :num-tests :seed :shrunk :result-data])
      (update :shrunk select-keys [:depth :total-nodes-visited :result-data])))

(defn render-analysis [analysis]
  (-> (render-quickcheck-results analysis)
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
