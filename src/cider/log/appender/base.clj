(ns cider.log.appender.base
  (:require [cider.log.protocol.appender :as p]))

(def ^:private default-size
  "The default size of the appender."
  100000)

(def ^:private default-threshold
  "The default threshold of the appender in percentage."
  10)

(defn- garbage-collect-events
  "Garbage collect some events of the `appender`."
  [{:keys [events event-index size threshold] :as appender}]
  (if (> (count event-index) (+ size (* size (/ threshold 100.0))))
    (assoc appender
           :events (take size events)
           :event-index (apply dissoc event-index (map :id (drop size events))))
    appender))

(defn- add-consumer [appender consumer]
  (update-in appender [:consumers (:id consumer)]
             (fn [old-consumer]
               (if old-consumer
                 (merge old-consumer (select-keys consumer [:filters]))
                 consumer))))

(defn- applicable-event?
  "Whether the `event` should be added to the appender."
  [{:keys [filter-fn]} event]
  (or (nil? filter-fn) (filter-fn event)))

(defn- append [appender event]
  (let [{:keys [filter-fn consumers]} appender]
    (if (applicable-event? appender event)
      (let [appender (-> (update appender :events #(cons event %))
                         (assoc-in [:event-index (:id event)] event)
                         (garbage-collect-events))]
        (doseq [{:keys [callback filter-fn] :as consumer} (vals consumers)
                :when (filter-fn event)]
          (callback consumer event))
        appender)
      appender)))

(defn- clear [appender]
  (assoc appender :events [] :event-index {}))

(defn- event [appender event-id]
  (get (:event-index appender) event-id))

(defn- remove-consumer [appender consumer]
  (update appender :consumers dissoc (:id consumer)))

(defn- update-consumer [appender consumer f]
  (update-in appender [:consumers (:id consumer)] f))

(defrecord BaseAppender [consumers id events event-index filter-fn filters size threshold]
  p/Appender
  (-add-consumer [appender consumer]
    (add-consumer appender consumer))
  (-append [appender event]
    (append appender event))
  (-clear [appender]
    (clear appender))
  (-consumers [_]
    (vals consumers))
  (-event [appender event-id]
    (event appender event-id))
  (-events [_]
    events)
  (-filters [_]
    filters)
  (-id [_]
    id)
  (-remove-consumer [appender consumer]
    (remove-consumer appender consumer))
  (-size [_]
    size)
  (-threshold [_]
    threshold)
  (-update-consumer [appender consumer f]
    (update-consumer appender consumer f)))

(defn make-appender
  "Make a base appender."
  [{:keys [id filters size threshold]}]
  (map->BaseAppender
   {:filters (or filters {})
    :id id
    :size (or size default-size)
    :threshold (or threshold default-threshold)}))
