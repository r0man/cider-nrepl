(ns cider.log.appender
  (:refer-clojure :exclude [name])
  (:require [cider.log.appender.base :as base]))

(defn make-appender
  "Make an atom appender."
  [appender]
  (atom (base/make-appender appender)))

(defn add-consumer
  "Add the `consumer` to the `appender`."
  [appender consumer]
  (swap! appender base/add-consumer consumer)
  appender)

(defn append
  "Append a logging `event` to the `appender`."
  [appender event]
  (swap! appender base/append event)
  appender)

(defn clear
  "Clear all logging `event` from the `appender`."
  [appender]
  (swap! appender base/clear)
  appender)

(defn consumers
  "Return the consumers of the `appender`."
  [appender]
  (vals (:consumers @appender)))

(defn consumer-by-id
  "Find the consumer of `appender` by `id`."
  [appender id]
  (some #(and (= id (:id %)) %) (consumers appender)))

(defn event
  "Lookup the event by `id` from the log `appender`."
  [appender id]
  (base/event @appender id))

(defn events
  "Return the events from the `appender`."
  [appender]
  (:events @appender))

(defn id
  "Return the id of the `appender`."
  [appender]
  (:id @appender))

(defn filters
  "Return the filters of the `appender`."
  [appender]
  (:filters @appender))

(defn size
  "Return the size of the `appender`."
  [appender]
  (:size @appender))

(defn threshold
  "Return the threshold of the `appender`."
  [appender]
  (:threshold @appender))

(defn remove-consumer
  "Remove the `consumer` from the `appender`."
  [appender consumer]
  (swap! appender base/remove-consumer consumer)
  appender)

(defn update-consumer
  "Update the `consumer` of the `appender` with `f`."
  [appender consumer f]
  (swap! appender base/update-consumer consumer f)
  appender)
