(ns cider.log.appender
  (:refer-clojure :exclude [name])
  (:require [cider.log.protocols :as p]))

(defrecord BaseAppender [consumers name events event-index level]
  p/Appender
  (-add-consumer [appender consumer]
    (assoc-in appender [:consumers (:name consumer)]
              {:name (:name consumer)
               :callback (:callback consumer)}))
  (-append [appender event]
    (doseq [{:keys [callback]} (vals consumers)]
      (callback event))
    (-> (update appender :events (fnil conj []) event)
        (assoc-in [:event-index (:id event)] event)))
  (-clear [appender]
    (assoc appender :events [] :event-index {}))
  (-consumers [_]
    (vals consumers))
  (-event [_ id]
    (get event-index id))
  (-events [_]
    events)
  (-level [_]
    level)
  (-name [_]
    name)
  (-remove-consumer [appender consumer]
    (update appender :consumers dissoc (:name consumer))))

(defrecord AtomAppender [name base]
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
  (-level [_]
    (p/-level @base))
  (-name [_]
    (p/-name @base))
  (-remove-consumer [appender consumer]
    (swap! base p/-remove-consumer consumer)
    appender))

(defn make-base-appender
  "Make a base appender."
  [{:keys [name level]}]
  (map->BaseAppender {:name name :level level}))

(defn make-atom-appender
  "Make an atom appender."
  [{:keys [name] :as appender}]
  (AtomAppender. name (atom (make-base-appender appender))))

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

(defn event
  "Return the event by `id` from the `appender`."
  [appender id]
  (p/-event appender id))

(defn events
  "Return the events from the `appender`."
  [appender]
  (p/-events appender))

(defn name
  "Return the name of the `appender`."
  [appender]
  (p/-name appender))

(defn level
  "Return the log level of the `appender`."
  [appender]
  (p/-level appender))

(defn remove-consumer
  "Remove the `consumer` from the `appender`."
  [appender consumer]
  (p/-remove-consumer appender consumer))
