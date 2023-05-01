(ns cider.log.framework.log4j2
  (:require [cider.log.appender :as appender]
            [clojure.set :as set]
            [clojure.string :as str])
  (:import (org.apache.logging.log4j Level MarkerManager ThreadContext)
           (org.apache.logging.log4j.core LogEvent LoggerContext)
           (org.apache.logging.log4j.core.appender AbstractAppender)
           (org.apache.logging.log4j.core.config AppenderRef LoggerConfig$Builder Property)
           (org.apache.logging.log4j.core.impl ThrowableProxy)
           (org.apache.logging.log4j.message MessageFormatMessage)))

(def ^:private log-levels
  "The standard log levels of the Log4j2 framework."
  (into {} (map (fn [^Level level]
                  [(keyword (str/lower-case (str level))) (.intLevel level)])
                [Level/TRACE Level/DEBUG Level/INFO Level/WARN Level/ERROR Level/FATAL])))

(def ^:private level-to-keyword
  {Level/TRACE :trace
   Level/DEBUG :debug
   Level/INFO  :info
   Level/WARN  :warn
   Level/ERROR :error
   Level/FATAL :fatal})

(def ^:private keyword-to-level
  (set/map-invert level-to-keyword))

(defn- event-exception [^LogEvent event]
  (let [proxy (.getThrownProxy event)]
    (when (instance? ThrowableProxy proxy)
      (.getThrowable ^ThrowableProxy proxy))))

(defn- event-data [^LogEvent event]
  (let [exception (event-exception event)]
    (cond-> {:arguments (-> event .getMessage .getParameters vec)
             :id (java.util.UUID/randomUUID)
             :level (level-to-keyword (.getLevel event))
             :logger (.getLoggerName event)
             :mdc (some->> (.getContextData event) .toMap (into {}))
             :message (some-> event .getMessage .getFormattedMessage)
             :thread (.getThreadName event)
             :timestamp (.getTimeMillis event)}
      exception (assoc :exception exception))))

(defn- add-appender [framework appender]
  (let [context (LoggerContext/getContext false)
        config (.getConfiguration context)]
    (locking config
      (let [logger (.getRootLogger context)
            logger-name (.getName logger)
            logger-config (.getLoggerConfig config logger-name)
            logger-config' (-> (LoggerConfig$Builder.)
                               (.withAdditivity (.isAdditive logger-config))
                               (.withConfig config)
                               (.withIncludeLocation (str (.isIncludeLocation logger-config)))
                               (.withLevel Level/INFO)
                               (.withLoggerName logger-name)
                               (.withProperties (.getPropertyArray logger-config))
                               (.withRefs (into-array
                                           (conj (seq (.getAppenderRefs logger-config))
                                                 (AppenderRef/createAppenderRef (:id @appender) nil nil))))
                               (.withtFilter (.getFilter logger-config))
                               (.build))
            instance (doto (proxy [AbstractAppender] [(:id @appender) nil nil true (into-array Property [])]
                             (append [^LogEvent event]
                               (appender/append appender (event-data event))))
                       (.start))]
        (swap! appender assoc :instance instance)
        (.addAppender logger-config' instance nil nil)
        (.addLogger config logger-name logger-config')
        (.updateLoggers context)
        framework))))

(defn- remove-appender
  "Remove `appender` from the Log4j `framework`."
  [framework appender]
  (let [context (LoggerContext/getContext false)
        config (.getConfiguration context)
        logger (.getRootLogger context)
        logger-name (.getName logger)
        logger-config (.getLoggerConfig config logger-name)]
    (.removeAppender logger-config (:id @appender))
    (.setConfiguration context config)
    (.updateLoggers context)
    framework))

(defn- log [{:keys [arguments exception level logger marker mdc message]}]
  (let [context (LoggerContext/getContext false)]
    (doseq [[key value] (seq mdc)]
      (ThreadContext/put key value))
    (.log (.getLogger context (or ^String logger ""))
          ^Level (keyword-to-level level Level/INFO)
          (some-> marker MarkerManager/getMarker)
          (MessageFormatMessage. ^String message (into-array Object arguments))
          ^Throwable exception)
    (when (seq mdc)
      (ThreadContext/clearAll))))

(def framework
  "The Log4j2 logging framework."
  {:add-appender-fn #'add-appender
   :constructor :cider.log.framework.lof4j2/framework
   :id "log4j2"
   :javadoc-url "https://logging.apache.org/log4j/2.x/javadoc/log4j-api"
   :levels log-levels
   :log-fn #'log
   :name "Log4j2"
   :remove-appender-fn #'remove-appender
   :website-url "https://logging.apache.org"})
