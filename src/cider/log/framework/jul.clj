(ns cider.log.framework.jul
  (:require [cider.log.appender :as appender]
            [cider.log.protocol.framework :as p]
            [clojure.set :as set]
            [clojure.string :as str])
  (:import [java.util.logging Level LogManager Logger LogRecord MemoryHandler StreamHandler]))

(def descriptor
  "The descriptor of the Java Util Logging framework."
  {:id :jul
   :name "Java Util Logging"
   :constructor :cider.log.framework.java/framework
   :description "The Java Logging APIs, contained in the package
    java.util.logging, facilitate software servicing and maintenance at customer
    sites by producing log reports suitable for analysis by end users, system
    administrators, field service engineers, and software development teams. The
    Logging APIs capture information such as security failures, configuration
    errors, performance bottlenecks, and/or bugs in the application or
    platform."
   :javadoc-url "https://docs.oracle.com/en/java/javase/19/core/java-logging-overview.html"
   :website-url "https://docs.oracle.com/javase/6/docs/technotes/guides/logging" })

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
  (let [atom-appender (appender/make-atom-appender appender)
        instance (proxy [StreamHandler] []
                   (publish [^LogRecord record]
                     (appender/append atom-appender (record->event record))))]
    (swap! (:base atom-appender) assoc :instance instance)
    (doto ^Logger (Logger/getLogger (or (:logger appender) ""))
      (.addHandler instance))
    (assoc-in framework [:appenders (:id appender)] atom-appender)))

(defn- remove-appender
  "Remove `appender` from the Logback `framework`."
  [framework appender]
  (let [logger (Logger/getLogger (or (:logger appender) ""))]
    (when-let [appender (get-in framework [:appenders (:id appender)])]
      (.removeHandler logger (:instance @(:base appender))))
    (update framework :appenders dissoc (:id appender))))

(defn- log [{:keys [logger] :as event}]
  (.log (Logger/getLogger (or logger "")) (event->record event)))

(defrecord Java [appenders]
  p/Framework
  (-appenders [_]
    (vals appenders))
  (-add-appender [framework appender]
    (add-appender framework appender))
  (-description [_]
    (:description descriptor))
  (-id [_]
    (:id descriptor))
  (-name [_]
    (:name descriptor))
  (-levels [_]
    log-levels)
  (-log [_ event]
    (log event))
  (-javadoc-url [_]
    (:javadoc-url descriptor))
  (-remove-appender [framework appender]
    (remove-appender framework appender))
  (-website-url [_]
    (:website-url descriptor)))

(defn framework []
  (map->Java descriptor))
