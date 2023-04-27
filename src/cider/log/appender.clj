(ns cider.log.appender
  (:refer-clojure :exclude [name])
  (:require [cider.log.protocol.appender :as p]))

(def ^:private default-size
  "The default size of the appender."
  100000)

(defn- free-space [{:keys [events event-index size threshold] :as appender}]
  (if (> (count event-index) (+ size (* size (/ threshold 100.0))))
    (assoc appender
           :events (take size events)
           :event-index (apply dissoc event-index (map :id (drop size events))))
    appender))

(defrecord BaseAppender [consumers id events event-index filter-fn filters size threshold]
  p/Appender
  (-add-consumer [appender consumer]
    (update-in appender [:consumers (:id consumer)]
               (fn [old-consumer]
                 (if old-consumer
                   (merge old-consumer (select-keys consumer [:filters]))
                   consumer))))
  (-append [appender event]
    (if (or (nil? filter-fn) (filter-fn event))
      (let [appender (-> (update appender :events #(cons event %))
                         (assoc-in [:event-index (:id event)] event)
                         (free-space))]
        (doseq [{:keys [callback filter-fn] :as consumer} (vals consumers)
                :when (filter-fn event)]
          (callback consumer event))
        appender)
      appender))
  (-clear [appender]
    (assoc appender :events [] :event-index {}))
  (-consumers [_]
    (vals consumers))
  (-event [_ id]
    (get event-index id))
  (-events [_]
    events)
  (-filters [_]
    filters)
  (-id [_]
    id)
  (-remove-consumer [appender consumer]
    (update appender :consumers dissoc (:id consumer)))
  (-size [_]
    size)
  (-threshold [_]
    threshold)
  (-update-consumer [appender consumer f]
    (update-in appender [:consumers (:id consumer)] f)))

(defrecord AtomAppender [base]
  p/Appender
  (-add-consumer [appender consumer]
    (swap! base p/-add-consumer consumer)
    appender)
  (-append [appender event]
    (swap! base p/-append event)
    appender)
  (-clear [appender]
    (swap! base p/-clear)
    appender)
  (-consumers [_]
    (p/-consumers @base))
  (-event [_ id]
    (p/-event @base id))
  (-events [_]
    (p/-events @base))
  (-filters [_]
    (p/-filters @base))
  (-id [_]
    (p/-id @base))
  (-remove-consumer [appender consumer]
    (swap! base p/-remove-consumer consumer)
    appender)
  (-size [_]
    (p/-size @base))
  (-threshold [_]
    (p/-threshold @base))
  (-update-consumer [appender consumer f]
    (swap! base p/-update-consumer consumer f)
    appender))

(defn make-base-appender
  "Make a base appender."
  [{:keys [id filters size threshold]}]
  (map->BaseAppender
   {:filters (or filters {})
    :id id
    :size (or size default-size)
    :threshold (or threshold 10)}))

(defn make-atom-appender
  "Make an atom appender."
  [appender]
  (AtomAppender. (atom (make-base-appender appender))))

(defn add-consumer
  "Add the `consumer` to the `appender`."
  [appender consumer]
  (p/-add-consumer appender consumer))

(defn append
  "Append a logging `event` to the `appender`."
  [appender event]
  (p/-append appender event))

(defn clear
  "Clear all logging `event` from the `appender`."
  [appender]
  (p/-clear appender))

(defn consumers
  "Return the consumers of the `appender`."
  [appender]
  (p/-consumers appender))

(defn consumer-by-id
  "Find the consumer of `appender` by `id`."
  [appender id]
  (some #(and (= id (:id %)) %) (consumers appender)))

(defn event
  "Lookup the event by `id` from the log `appender`."
  [appender id]
  (p/-event appender id))

(defn events
  "Return the events from the `appender`."
  [appender]
  (p/-events appender))

(defn id
  "Return the id of the `appender`."
  [appender]
  (p/-id appender))

(defn filters
  "Return the filters of the `appender`."
  [appender]
  (p/-filters appender))

(defn size
  "Return the size of the `appender`."
  [appender]
  (p/-size appender))

(defn threshold
  "Return the threshold of the `appender`."
  [appender]
  (p/-threshold appender))

(defn remove-consumer
  "Remove the `consumer` from the `appender`."
  [appender consumer]
  (p/-remove-consumer appender consumer))

(defn update-consumer
  "Update the `consumer` of the `appender` with `f`."
  [appender consumer f]
  (p/-update-consumer appender consumer f))
