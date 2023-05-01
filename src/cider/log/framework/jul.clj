(ns cider.log.framework.jul
  (:require [cider.log.appender :as appender]
            [clojure.set :as set]
            [clojure.string :as str])
  (:import (java.util.logging Level Logger LogRecord StreamHandler)))

(def ^:private log-levels
  "The standard log levels of the Java Logging framework."
  (into {} (map (fn [^Level level]
                  [(keyword (str/lower-case (str level)))
                   (.intValue level)])
                [Level/FINEST
                 Level/FINER
                 Level/FINE
                 Level/CONFIG
                 Level/INFO
                 Level/WARNING
                 Level/SEVERE])))

(def ^:private level-to-keyword
  {Level/FINEST :finest
   Level/FINER :finer
   Level/FINE :fine
   Level/CONFIG :config
   Level/INFO  :info
   Level/WARNING :warning
   Level/SEVERE :severe})

(def ^:private keyword-to-level
  (set/map-invert level-to-keyword))

(defn- event->record
  "Convert a Cider log event into a Java LogRecord."
  ^LogRecord [{:keys [arguments exception level logger message]}]
  (doto (LogRecord. (keyword-to-level (or level :info)) message)
    (.setLoggerName (or logger ""))
    (.setParameters (into-array Object arguments))
    (.setThrown exception)))

(defn- thread-by-id
  "Find the thread by `id`."
  ^Thread [id]
  (some #(and (= id (.getId ^Thread %)) %)
        (keys (Thread/getAllStackTraces))))

(defn- record->event
  "Convert a Java LogRecord into a Cider log event."
  [^LogRecord record]
  (let [exception (.getThrown record)]
    (cond-> {:arguments (vec (.getParameters record))
             :id (java.util.UUID/randomUUID)
             :level (level-to-keyword (.getLevel record))
             :logger (.getLoggerName record)
             :mdc {}
             :message (.getMessage record)
             :thread (some-> (.getThreadID record) thread-by-id .getName)
             :timestamp (.getMillis record)}
      exception (assoc :exception exception))))

(defn- add-appender
  "Attach the Logback appender."
  [framework appender]
  (let [instance (proxy [StreamHandler] []
                   (publish [^LogRecord record]
                     (appender/append appender (record->event record))))]
    (swap! appender assoc :instance instance)
    (doto ^Logger (Logger/getLogger (or (:logger appender) ""))
      (.addHandler instance))
    framework))

(defn- remove-appender
  "Remove `appender` from the Logback `framework`."
  [framework appender]
  (let [logger (Logger/getLogger (or (:logger appender) ""))]
    (.removeHandler logger (:instance @appender))
    framework))

(defn- log [{:keys [logger] :as event}]
  (.log (Logger/getLogger (or logger "")) (event->record event)))

(def framework
  "The Java Util Logging framework."
  {:add-appender-fn #'add-appender
   :constructor :cider.log.framework.java/framework
   :id "jul"
   :javadoc-url "https://docs.oracle.com/en/java/javase/19/docs/api/java.logging/java/util/logging/package-summary.html"
   :levels log-levels
   :log-fn #'log
   :name "Java Util Logging"
   :remove-appender-fn #'remove-appender
   :website-url "https://docs.oracle.com/en/java/javase/19/core/java-logging-overview.html"})
