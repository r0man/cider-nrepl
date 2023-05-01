(ns cider.log.appender.base)

(def ^:private default-size
  "The default size of the appender."
  100000)

(def ^:private default-threshold
  "The default threshold of the appender in percentage."
  10)

(defn- garbage-collect?
  "Whether to garbage collect events, or not."
  [{:keys [event-index size threshold]}]
  (> (count event-index) (+ size (* size (/ threshold 100.0)))))

(defn- garbage-collect-events
  "Garbage collect some events of the `appender`."
  [{:keys [events event-index size] :as appender}]
  (if (garbage-collect? appender)
    (assoc appender
           :events (take size events)
           :event-index (apply dissoc event-index (map :id (drop size events))))
    appender))

(defn add-consumer [appender consumer]
  (assert (not (get-in appender [:consumers (:id consumer)]))
          (format "Consumer %s already registered" (:id consumer)))
  (assoc-in appender [:consumers (:id consumer)] consumer))

(defn- add-event?
  "Whether the `event` should be added to the appender."
  [{:keys [filter-fn]} event]
  (or (nil? filter-fn) (filter-fn event)))

(defn- notify-consumers
  [{:keys [consumers] :as appender} event]
  (doseq [{:keys [callback filter-fn] :as consumer} (vals consumers)
          :when (filter-fn event)]
    (callback consumer event))
  appender)

(defn- enqueue-event
  "Enqueue the `event` to the event list of `appender`."
  [appender event]
  (update appender :events #(cons event %)))

(defn- index-event
  "Add the `event` to the index of `appender`."
  [appender event]
  (assoc-in appender [:event-index (:id event)] event))

(defn append
  "Add the `event` to the `appender`."
  [appender event]
  (if (add-event? appender event)
    (-> (enqueue-event appender event)
        (index-event event)
        (notify-consumers event)
        (garbage-collect-events))
    appender))

(defn clear [appender]
  (assoc appender :events [] :event-index {}))

(defn event [appender event-id]
  (get (:event-index appender) event-id))

(defn events [appender]
  (take (:size appender) (:events appender)))

(defn remove-consumer [appender consumer]
  (update appender :consumers dissoc (:id consumer)))

(defn update-consumer [appender consumer f]
  (update-in appender [:consumers (:id consumer)] f))

(defn make-appender
  "Make a base appender."
  [& [{:keys [id filters size threshold]}]]
  {:filters (or filters {})
   :event-index {}
   :events nil
   :id id
   :size (or size default-size)
   :threshold (or threshold default-threshold)})
