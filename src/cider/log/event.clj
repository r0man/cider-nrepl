(ns cider.log.event
  (:require [clojure.string :as str]))

(defn- exception-name [^Class exception]
  (some-> exception .getClass .getName))

(defn exception-frequencies [events]
  (frequencies (remove nil? (map #(some-> % :exception exception-name) events))))

(defn logger-frequencies [events]
  (frequencies (map :logger events)))

(defn level-frequencies [events]
  (frequencies (map :level events)))

(defn inspect [events event-id]
  (some #(and (= event-id (:id %)) %) events))

(defn search-filter
  [{:keys [end-time exceptions levels loggers pattern start-time threads]}]
  (let [exceptions (set exceptions)
        levels (set (map (comp keyword str/lower-case) levels))
        loggers (set loggers)
        threads (set threads)
        pattern (try (some-> pattern re-pattern) (catch Exception _))]
    (if (or (seq exceptions) (seq levels) (seq loggers) (seq threads) pattern start-time end-time)
      (fn [event]
        (and (or (empty? exceptions)
                 (contains? exceptions (some-> event :exception exception-name)))
             (or (empty? levels)
                 (contains? levels (:level event)))
             (or (empty? loggers)
                 (contains? loggers (:logger event)))
             (or (empty? threads)
                 (contains? threads (:thread event)))
             (or (not pattern)
                 (some->> event :message (re-matches pattern)))
             (or (not (pos-int? start-time))
                 (>= (:timestamp event) start-time))
             (or (not (pos-int? end-time))
                 (< (:timestamp event) end-time))))
      (constantly true))))

(defn search [events opts]
  (->> events
       (filter (search-filter (:filters opts)))
       (drop (or (:offset opts) 0))
       (take (or (:limit opts) 500))))

(defn thread-frequencies [events]
  (frequencies (map (comp name :thread) events)))
