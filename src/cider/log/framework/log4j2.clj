(ns cider.log.framework.log4j2
  (:require [cider.log.appender :as appender]
            [cider.log.protocol.framework :as p]
            [clojure.set :as set]
            [clojure.string :as str])
  (:import [org.apache.logging.log4j Level LogManager MarkerManager]
           [org.apache.logging.log4j.core LogEvent LoggerContext]
           [org.apache.logging.log4j.core.appender AbstractAppender]
           [org.apache.logging.log4j.core.config AppenderRef LoggerConfig LoggerConfig$Builder Property]
           [org.apache.logging.log4j.core.impl ThrowableProxy]
           [org.apache.logging.log4j.message MessageFormatMessage]))

(def descriptor
  "The descriptor of the Log4j2 logging framework."
  {:id :log4j2
   :name "Log4j2"
   :constructor :cider.log.framework.lof4j2/framework
   :description "Log4j 2 provides both a portable logging API and implementation for Java
    with significant improvements over its predecessor, Log4j 1.x."
   :javadoc-url "https://logging.apache.org/log4j/2.x/javadoc/log4j-api"
   :website-url "https://logging.apache.org"})

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
                                                 (AppenderRef/createAppenderRef (:id appender) nil nil))))
                               (.withtFilter (.getFilter logger-config))
                               (.build))
            atom-appender (appender/make-atom-appender appender)
            instance (doto (proxy [AbstractAppender] [(:id appender) nil nil true (into-array Property [])]
                             (append [^LogEvent event]
                               (appender/append atom-appender (event-data event))))
                       (.start))]
        (swap! (:base atom-appender) assoc :instance instance)
        (.addAppender logger-config' instance nil nil)
        (.addLogger config logger-name logger-config')
        (.updateLoggers context)
        (assoc-in framework [:appenders (:id appender)] atom-appender)))))

(defn- remove-appender
  "Remove `appender` from the Log4j `framework`."
  [framework appender]
  (when-let [appender (get-in framework [:appenders (:id appender)])]
    (let [context (LoggerContext/getContext false)
          config (.getConfiguration context)
          logger (.getRootLogger context)
          logger-name (.getName logger)
          logger-config (.getLoggerConfig config logger-name)]
      (.removeAppender logger-config (:id appender))
      (.setConfiguration context config)
      (.updateLoggers context)))
  (update framework :appenders dissoc (:id appender)))

(defn- log [{:keys [arguments exception level logger marker message]}]
  (let [context (LoggerContext/getContext false)]
    (.log (.getLogger context (or ^String logger ""))
          ^Level (keyword-to-level level Level/INFO)
          (some-> marker MarkerManager/getMarker)
          (MessageFormatMessage. ^String message (into-array Object arguments))
          ^Throwable exception)))

(defrecord Log4j [name appenders]
  p/Framework
  (-appenders [_]
    (vals appenders))
  (-add-appender [framework appender]
    (add-appender framework appender))
  (-description [_]
    "Log4j 2 provides both a portable logging API and implementation for Java
    with significant improvements over its predecessor, Log4j 1.x.")
  (-id [_]
    :log4j2)
  (-name [_]
    "Log4j2")
  (-levels [_]
    log-levels)
  (-log [_ event]
    (log event))
  (-javadoc-url [_]
    "https://logging.apache.org/log4j/2.x/javadoc/log4j-api")
  (-remove-appender [framework appender]
    (remove-appender framework appender))
  (-website-url [_]
    "https://logging.apache.org"))

(defn framework []
  (map->Log4j {:id :log4j2
               :name "Log4j2"
               :description "The Apache Log4j2 logging framework."
               :appenders {}}))
