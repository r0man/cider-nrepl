(ns stateful-check.debugger.render)

(defn- render-value [value]
  (select-keys value [:cursor :index :rendered]))

(defn- render-argument [command]
  (select-keys command [:cursor :index :name :rendered]))

(defn- render-command [command]
  (select-keys command [:cursor :index :name :rendered]))

(defn- render-result [value]
  (select-keys value [:cursor :index :exception :rendered]))

(defn- render-execution
  [{:keys [arguments command failures handle result state]}]
  {:arguments (mapv render-argument arguments)
   :command (render-command command)
   :failures failures
   :handle (render-value handle)
   :result (render-result result)
   :state (render-value state)})

(defn- render-sequential [commands]
  (mapv render-execution commands))

(defn- render-parallel [commands]
  (mapv render-sequential commands))

(defn- render-executions [{:keys [sequential parallel] :as execution}]
  (-> (select-keys execution [:specification :options])
      (assoc :sequential (render-sequential sequential))
      (assoc :parallel (render-parallel parallel))))

(defn- render-executions-index [executions]
  (into {} (for [[id execution] executions]
             [id (render-executions execution)])))

(defn- render-quickcheck-results [results]
  (-> (select-keys results [:failing-size :frequencies :num-tests :seed])
      (update :shrunk select-keys [:depth :total-nodes-visited] )))

(defn- render-analysis
  [{:keys [executions results] :as report}]
  (-> (select-keys report [:id :specification :options :ns :var])
      (assoc :results (render-quickcheck-results results))
      (assoc :executions (render-executions-index executions))))

(defn- render-results
  [results]
  (into {} (for [[id analysis] results] [id (render-analysis analysis)])))

(defn render-debugger
  "Render the `debugger` in a Bencode compatible format."
  [debugger]
  (update debugger :results render-results))
