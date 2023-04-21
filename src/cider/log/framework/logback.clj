(ns cider.log.framework.logback
  (:require [cider.log.appender :as appender]
            [cider.log.protocol.framework :as p]
            [clojure.set :as set])
  (:import [ch.qos.logback.classic Level Logger LoggerContext]
           [ch.qos.logback.classic.spi ILoggingEvent LoggingEvent ThrowableProxy]
           [ch.qos.logback.core Appender AppenderBase]
           [org.slf4j LoggerFactory Marker MarkerFactory]))

(defn- logger-context
  "Return the Logback logger context."
  ^LoggerContext []
  (LoggerFactory/getILoggerFactory))

(defn- get-logger
  "Return the logger by `name` from the logger `context`."
  ^Logger [^String name]
  (.getLogger (logger-context) name))

(def ^:private level-to-keyword
  {Level/TRACE :trace
   Level/DEBUG :debug
   Level/INFO  :info
   Level/WARN  :warn
   Level/ERROR :error})

(def ^:private keyword-to-level
  (set/map-invert level-to-keyword))

(defn- event-exception [^LoggingEvent event]
  (let [proxy (.getThrowableProxy event)]
    (when (instance? ThrowableProxy proxy)
      (.getThrowable ^ThrowableProxy proxy))))

(defn- event-data [^LoggingEvent event]
  (let [exception (event-exception event)]
    (cond-> {:data (vec (.getArgumentArray event))
             :id (java.util.UUID/randomUUID)
             :level (level-to-keyword (.getLevel event))
             :logger (.getLoggerName event)
             :mdc (into {} (.getMDCPropertyMap event))
             :message (.getFormattedMessage event)
             :thread (.getThreadName event)
             :timestamp (.getTimeStamp event)}
      exception (assoc :exception exception))))

(defn- add-appender
  "Attach the Logback appender."
  [framework appender]
  (let [atom-appender (appender/make-atom-appender appender)
        instance (doto (proxy [AppenderBase] []
                         (append [^ILoggingEvent event]
                           (appender/append atom-appender (event-data event))))
                   (.setName (:name appender))
                   (.start))]
    (swap! (:base atom-appender) assoc :instance instance)
    (doto ^Logger (get-logger Logger/ROOT_LOGGER_NAME)
      (.addAppender instance))
    (assoc-in framework [:appenders (:name appender)] atom-appender)))

(defn- level-int [level]
  (some-> level keyword-to-level Level/toLocationAwareLoggerInteger))

(defn- log [framework {:keys [arguments exception level logger marker message]}]
  (let [logger (get-logger (or logger Logger/ROOT_LOGGER_NAME))]
    (.log logger
          (some-> marker MarkerFactory/getMarker)
          ^String (.getName logger) ;;TODO: What is "fqcn"?
          (level-int (or level :info))
          message
          (into-array Object arguments)
          exception)))

(defn- remove-appender
  "Remove `appender` from the Logback `framework`."
  [framework appender]
  (when-let [appender (get-in framework [:appenders (:name appender)])]
    (.stop ^Appender (:instance @(:base appender))))
  (doto ^Logger (get-logger Logger/ROOT_LOGGER_NAME)
    (.detachAppender ^String (:name appender)))
  (update framework :appenders dissoc (:name appender)))

(defrecord Logback [name appenders]
  p/Framework
  (-appenders [_]
    (vals appenders))
  (-add-appender [framework appender]
    (add-appender framework appender))
  (-description [_]
    "Logback is intended as a successor to the popular log4j project, picking up
    where log4j 1.x leaves off.")
  (-id [_]
    "logback")
  (-name [_]
    "Logback")
  (-log [framework message]
    (log framework message))
  (-javadoc-url [_]
    "https://logback.qos.ch/apidocs")
  (-remove-appender [framework appender]
    (remove-appender framework appender))
  (-website-url [_]
    "https://logback.qos.ch/"))

(defn framework []
  (map->Logback {:id :logback
                 :name "Logback"
                 :description "The Logback logging framework."
                 :appenders {}}))
