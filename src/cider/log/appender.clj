(ns cider.log.appender
  (:refer-clojure :exclude [name])
  (:require [cider.log.appender.atom :as atom]
            [cider.log.appender.base :as base]
            [cider.log.protocol.appender :as p]))

(defn make-atom-appender
  "Make an atom appender."
  [appender]
  (atom/make-appender appender))

(defn make-base-appender
  "Make a base appender."
  [appender]
  (base/make-appender appender))

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
