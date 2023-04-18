(ns cider.log.framework.logback
  (:require [clojure.set :as set]
            [cider.log.appender :as appender]
            [cider.log.framework :as framework]
            [cider.log.protocols :as p])
  (:import [ch.qos.logback.classic Level Logger LoggerContext]
           [ch.qos.logback.classic.spi ILoggingEvent LoggingEvent ThrowableProxy]
           [ch.qos.logback.core AppenderBase]
           [org.slf4j LoggerFactory MarkerFactory]))

(defn- logger-context
  "Return the Logback logger context."
  ^LoggerContext []
  (LoggerFactory/getILoggerFactory))

(defn- root-logger
  "Return the Logback root logger."
  ^Logger [^LoggerContext context]
  (.getLogger context Logger/ROOT_LOGGER_NAME))

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
  "Attach the Logback appenders."
  [framework appender]
  (let [atom-appender (appender/make-atom-appender appender)
        instance (doto (proxy [AppenderBase] []
                         (append [^ILoggingEvent event]
                           (appender/append atom-appender (event-data event))))
                   (.setName (:name appender))
                   (.start))]
    (swap! (:base atom-appender) assoc :instance instance)
    (doto ^Logger (root-logger (:context framework))
      (.addAppender instance))
    (assoc-in framework [:appenders (:name appender)] atom-appender)))

(defn- level-int [level]
  (some-> level keyword-to-level Level/toLocationAwareLoggerInteger))

(defn- log [framework {:keys [arguments exception level logger marker message]}]
  (let [logger-instance (.getLogger (:context framework) (or logger Logger/ROOT_LOGGER_NAME))]
    (.log logger-instance
          (some-> marker MarkerFactory/getMarker)
          (.getName logger-instance) ;;TODO: What is "fqcn"?
          (level-int (or level :info))
          message
          (into-array Object arguments)
          exception)))

(defn- remove-appender
  "Remove `appender` from the Logback `framework`."
  [framework appender]
  (when-let [appender (get-in framework [:appenders (:name appender)])]
    (.stop (:instance @(:base appender))))
  (doto ^Logger (root-logger (:context framework))
    (.detachAppender ^String (:name appender)))
  (update framework :appenders dissoc (:name appender)))

(defrecord Logback [name appenders]
  p/Framework
  (-appenders [framework]
    (vals appenders))
  (-add-appender [framework appender]
    (add-appender framework appender))
  (-log [framework message]
    (log framework message))
  (-remove-appender [framework appender]
    (remove-appender framework appender))
  (-update-appender [framework appender]))

(defn framework []
  (map->Logback {:id :logback
                 :name "Logback"
                 :description "The Logback logging framework."
                 :context (logger-context)
                 :appenders {}}))
