(ns cider.log.framework.log4j2
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.set :as set]
            [cider.log.appender :as appender]
            [cider.log.framework :as framework]
            [cider.log.protocols :as p])
  (:import [org.apache.logging.log4j LogManager MarkerManager]
           [org.apache.logging.log4j Level]
           [org.apache.logging.log4j.core LogEvent]
           [org.apache.logging.log4j.core.appender FileAppender]
           [org.apache.logging.log4j.core.appender AbstractAppender]
           [org.apache.logging.log4j.core.config AppenderRef LoggerConfig]
           [org.apache.logging.log4j.core.config Property]
           [org.apache.logging.log4j.core.impl ThrowableProxy]
           [org.apache.logging.log4j.core.layout PatternLayout]
           [org.apache.logging.log4j.spi LoggerContext]))

(defn- logger-context ^LoggerContext []
  (LogManager/getContext false))

(def ^:private level-to-keyword
  {Level/TRACE :trace
   Level/DEBUG :debug
   Level/INFO  :info
   Level/WARN  :warn
   Level/ERROR :error})

(def ^:private keyword-to-level
  (set/map-invert level-to-keyword))

(defn- event-exception [^LogEvent event]
  (let [proxy (.getThrownProxy event)]
    (when (instance? ThrowableProxy proxy)
      (.getThrowable ^ThrowableProxy proxy))))

(defn- event-data [^LogEvent event]
  (let [exception (event-exception event)]
    (cond-> {:data (some-> event .getMessage .getParameters vec)
             :id (java.util.UUID/randomUUID)
             :level (level-to-keyword (.getLevel event))
             :logger (.getLoggerName event)
             ;; :mdc (into {} (.getMDCPropertyMap event))
             :message (some-> event .getMessage .getFormattedMessage)
             :thread (.getThreadName event)
             :timestamp (.getTimeMillis event)}
      exception (assoc :exception exception))))

;; if (arg[0].equals("log") ) {
;;   org.apache.logging.log4j.Logger logger
;;     = org.apache.logging.log4j.LogManager.getLogger("loggerNameFromXMLConfig");
;;   org.apache.logging.log4j.core.Logger coreLogger
;;     = (org.apache.logging.log4j.core.Logger)logger;
;;   org.apache.logging.log4j.core.LoggerContext context
;;     = (org.apache.logging.log4j.core.LoggerContext)coreLogger.getContext();
;;   org.apache.logging.log4j.core.config.BaseConfiguration configuration
;;     = (org.apache.logging.log4j.core.config.BaseConfiguration)context.getConfiguration();

;;   coreLogger.addAppender(configuration.getAppender("appenderNameFromXMLConfig"));
;; } else {
;;   //...
;; }

;; final LoggerContext ctx = (LoggerContext) LogManager.getContext(false);
;; final Configuration config = ctx.getConfiguration();
;; final Layout layout = PatternLayout.createDefaultLayout(config);
;; Appender appender = FileAppender.createAppender("target/test.log", "false", "false", "File", "true",
;;                                                 "false", "false", "4000", layout, null, "false", null, config);
;; appender.start();
;; config.addAppender(appender);
;; AppenderRef ref = AppenderRef.createAppenderRef("File", null, null);
;; AppenderRef[] refs = new AppenderRef[] {ref};
;; LoggerConfig loggerConfig = LoggerConfig.createLogger("false", "info", "org.apache.logging.log4j",
;;                                                       "true", refs, null, config, null );
;; loggerConfig.addAppender(appender, null, null);
;; config.addLogger("org.apache.logging.log4j", loggerConfig);
;; ctx.updateLoggers();

(defn- add-appender [framework appender]
  (let [context (LogManager/getContext false)
        config (.getConfiguration context)
        layout (PatternLayout/createDefaultLayout config)
        instance (doto (FileAppender/createAppender
                        "target/test.log" "false" "false" (:name appender) "true" "false"
                        "false" "4000" layout nil "false" nil config)
                   (.start))
        _ (.addAppender config instance)
        refs (into-array [(AppenderRef/createAppenderRef (:name appender) nil nil)])
        logger-config (LoggerConfig/createLogger
                       false Level/INFO "org.apache.logging.log4j"
                       "true" refs nil config nil)]
    (.addAppender logger-config instance nil nil)
    (.addLogger config "org.apache.logging.log4j" logger-config)
    (def my-logger-config logger-config)
    (def my-config config)
    (.updateLoggers context)
    (.info (LogManager/getLogger "org.apache.logging.log4j") "HELLO WORLD 1")
    (.info (LogManager/getLogger "org.apache.logging.log4j") "HELLO WORLD 2")
    (assoc-in framework [:appenders (:name appender)]
              {:name (:name appender)
               :instance instance
               :state (atom {})})))

(defn- remove-appender
  "Remove `appender` from the Log4j `framework`."
  [framework appender]
  (prn "REMOVE APPENDER")
  (when-let [appender (get-in framework [:appenders (:name appender)])]
    (let [context (LogManager/getContext false)
          config (.getConfiguration context)
          logger-config (.getLoggerConfig config "org.apache.logging.log4j")]
      (prn logger-config)
      (.removeAppender logger-config (:name appender))
      (.stop (:instance appender))
      (.removeLogger config "org.apache.logging.log4j")
      (.updateLoggers context)
      ;; (.removeAppender (LogManager/getRootLogger) (:instance appender))
      ))
  (update framework :appenders dissoc (:name appender)))

#_(defn- add-appender
    "Add `appender` to the Log4j `framework`."
    [framework appender]
    (let [state (atom {:consumers []
                       :events []
                       :event-index {}})
          instance (doto (proxy [AbstractAppender] [(:name appender) nil nil true (into-array Property [])]
                           (append [^LogEvent event]
                             (prn "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
                             (let [event (event-data event)]
                               (swap! state (fn [state]
                                              (-> (update state :events (fnil conj []) event)
                                                  (assoc-in [:event-index (:id event)] event)))))))
                     (.start))]
      (let [context (LogManager/getContext false)
            logger (.getLogger context (.getName (LogManager/getRootLogger)))
            config (.getConfiguration context)
            logger-config (.getLoggerConfig config (.getName logger))
            appenders (vals (.getAppenders logger-config))
            appender-refs (.getAppenderRefs logger-config)
            appender-ref (AppenderRef/createAppenderRef (:name appender) nil nil)
            new-logger-config (LoggerConfig/createLogger
                               (.isAdditive logger-config)
                               (.getLevel logger-config)
                               (.getName logger-config)
                               (str (.isIncludeLocation logger-config))
                               (into-array (cons appender-ref appender-refs))
                               nil
                               config
                               nil)]
        (doseq [appender (cons instance appenders)]
          (.addAppender new-logger-config appender nil nil))
        (prn "NEW APPENDER" (.getAppenders new-logger-config))
        (prn "NEW REFS" (.getAppenderRefs new-logger-config))
        (.addLogger config (.getName logger) new-logger-config)
        (.addAppender (LogManager/getRootLogger)
                      (.getAppender (.getConfiguration context)
                                    (.getName instance)))
        (println (str config))
        (.info (LogManager/getLogger "x") "XXXX")
        (.updateLoggers context)
        (.info (LogManager/getLogger "x") "XXXX")
        (assoc-in framework [:appenders (:name appender)]
                  {:name (:name appender)
                   :instance instance
                   :state state}))))

;; (defn- add-appender
;;   "Add `appender` to the Log4j `framework`."
;;   [framework appender]
;;   (let [state (atom {:consumers []
;;                      :events []
;;                      :event-index {}})
;;         instance (doto (proxy [AbstractAppender] [(:name appender) nil nil true (into-array Property [])]
;;                          (append [^LogEvent event]
;;                            (prn "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
;;                            (let [event (event-data event)]
;;                              (swap! state (fn [state]
;;                                             (-> (update state :events (fnil conj []) event)
;;                                                 (assoc-in [:event-index (:id event)] event)))))))
;;                    (.start))]
;;     (let [context (LogManager/getContext false)
;;           logger (LogManager/getLogger "")
;;           core-logger (cast org.apache.logging.log4j.core.Logger logger)
;;           context (.getContext core-logger)
;;           configuration (.getConfiguration context)]
;;       (.addAppender configuration instance)
;;       (.addAppender core-logger (.getAppender configuration (.getName instance)))
;;       (.updateLoggers context)
;;       ;; (.info (LogManager/getLogger "x") "XXXX")
;;       (prn logger)
;;       (prn core-logger)
;;       (assoc-in framework [:appenders (:name appender)]
;;                 {:name (:name appender)
;;                  :instance instance
;;                  :state state}))))

;; (defn- add-appender
;;   "Add `appender` to the Log4j `framework`."
;;   [framework appender]
;;   (let [state (atom {:consumers []
;;                      :events []
;;                      :event-index {}})
;;         instance (doto (proxy [AbstractAppender] [(:name appender) nil nil true (into-array Property [])]
;;                          (append [^LogEvent event]
;;                            (prn "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
;;                            (let [event (event-data event)]
;;                              (swap! state (fn [state]
;;                                             (-> (update state :events (fnil conj []) event)
;;                                                 (assoc-in [:event-index (:id event)] event)))))))
;;                    (.start))]
;;     (prn [:INSTANCE instance])
;;     (prn (.getAppenders (LogManager/getRootLogger)))
;;     (.addAppender (LogManager/getRootLogger) instance)
;;     ;; (.info (LogManager/getLogger "x") "XXXX")
;;     ;; (log framework {:message ":::::::::::::::::::::::::::::::::::"})
;;     ;; (prn (.getAppenders (LogManager/getRootLogger)))
;;     (assoc-in framework [:appenders (:name appender)]
;;               {:name (:name appender)
;;                :instance instance
;;                :state state})))

;; (defn get-loggers
;;   ([]
;;    (get-loggers (LogManager/getContext false)))
;;   ([context]
;;    (->> (.getLoggers (.getConfiguration context))
;;         keys
;;         (map (fn [logger-name]
;;                (.getLogger context logger-name))))))

;; (defn add-appender-to-running-context
;;   ([appender]
;;    (add-appender-to-running-context (LogManager/getContext false) appender))
;;   ([^LoggerContext context appender]
;;    (-> (.getConfiguration context)
;;        (.addAppender appender))
;;    (let [appender-from-ctx (-> (.getConfiguration context)
;;                                (.getAppender (.getName appender)))]
;;      (doseq [logger (get-loggers context)]
;;        (prn (str "adding appender to " (.getName logger)))
;;        (.addAppender logger appender-from-ctx)))
;;    (.updateLoggers context)))

;; (defn- add-appender
;;   "Add `appender` to the Log4j `framework`."
;;   [framework appender]
;;   (prn "ADD APPENDER")
;;   (let [state (atom {:consumers []
;;                      :events []
;;                      :event-index {}})
;;         instance (doto (proxy [AbstractAppender] [(:name appender) nil nil true (into-array Property [])]
;;                          (append [^LogEvent event]
;;                            (prn "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
;;                            (let [event (event-data event)]
;;                              (swap! state (fn [state]
;;                                             (-> (update state :events (fnil conj []) event)
;;                                                 (assoc-in [:event-index (:id event)] event)))))))
;;                    (.start))]
;;     (let [context (LogManager/getContext false)
;;           ;; logger ""
;;           ;; config (.getConfiguration context)
;;           ;; logger-config (.getLoggerConfig config logger)
;;           ;; appenders (vals (.getAppenders logger-config))
;;           ;; appender-refs (.getAppenderRefs logger-config)
;;           ;; appender-ref (AppenderRef/createAppenderRef (:name appender) nil nil)
;;           ;; new-logger-config (LoggerConfig/createLogger
;;           ;;                    (.isAdditive logger-config)
;;           ;;                    (.getLevel logger-config)
;;           ;;                    (.getName logger-config)
;;           ;;                    (str (.isIncludeLocation logger-config))
;;           ;;                    (into-array (cons appender-ref appender-refs))
;;           ;;                    nil
;;           ;;                    config
;;           ;;                    nil)
;;           ]
;;       ;; (def my-config logger-config)
;;       ;; (.getAppenders my-config)
;;       ;; (.addAppender logger-config instance Level/INFO nil)
;;       ;; (doseq [appender (cons instance appenders)]
;;       ;;   (.addAppender new-logger-config appender nil nil))
;;       ;; (prn "NEW APPENDER" (.getAppenders new-logger-config))
;;       ;; (prn "NEW REFS" (.getAppenderRefs new-logger-config))
;;       ;; (.addLogger config logger new-logger-config)
;;       ;; (.addAppender (LogManager/getRootLogger)
;;       ;;               (.getAppender (.getConfiguration context)
;;       ;;                             (.getName instance)))
;;       ;; (println (str config))
;;       ;; (.updateLoggers context)
;;       (add-appender-to-running-context context instance)
;;       (.info (org.apache.logging.log4j.LogManager/getLogger "") "A")
;;       (.info (org.apache.logging.log4j.LogManager/getLogger "") "B")
;;       ;; (.info (LogManager/getLogger "") "XXXX")
;;       (assoc-in framework [:appenders (:name appender)]
;;                 {:name (:name appender)
;;                  :instance instance
;;                  :state state}))))

;; (defn- add-appender
;;   "Add `appender` to the Log4j `framework`."
;;   [framework appender]
;;   (let [state (atom {:consumers []
;;                      :events []
;;                      :event-index {}})
;;         instance (doto (proxy [AbstractAppender] [(:name appender) nil nil true (into-array Property [])]
;;                          (append [^LogEvent event]
;;                            (prn "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
;;                            (let [event (event-data event)]
;;                              (swap! state (fn [state]
;;                                             (-> (update state :events (fnil conj []) event)
;;                                                 (assoc-in [:event-index (:id event)] event)))))))
;;                    (.start))]
;;     (let [context (LogManager/getContext false)
;;           logger (.getLogger context (.getName (LogManager/getRootLogger)))
;;           config (.getConfiguration context)]
;;       (.stop config)
;;       (.addLoggerAppender config logger instance)
;;       (.start config)
;;       (.info logger "XXXX")
;;       (assoc-in framework [:appenders (:name appender)]
;;                 {:name (:name appender)
;;                  :instance instance
;;                  :state state}))))

#_(defn- remove-appender
    "Remove `appender` from the Log4j `framework`."
    [framework appender]
    (prn "REMOVE APPENDER")
    (when-let [appender (get-in framework [:appenders (:name appender)])]
      (.stop (:instance appender))
      (.removeAppender (LogManager/getRootLogger) (:instance appender)))
    (update framework :appenders dissoc (:name appender)))

;; (.getName (LogManager/getRootLogger))

(defn- log [framework {:keys [arguments level logger marker message]}]
  ;; (.info (LogManager/getLogger "") message)
  (.info (LogManager/getLogger (or logger "")) message)
  ;; (.info (LogManager/getLogger logger) message)
  ;; (.log (LogManager/getLogger (or logger ""))
  ;;       (keyword-to-level (or level :info))
  ;;       (some-> marker MarkerManager/getMarker)
  ;;       message
  ;;       (into-array Object arguments))
  )

(defrecord Log4j [name appenders]
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
  (map->Log4j {:id :log4j2
               :name "Log4j2"
               :description "The Apache Log4j2 logging framework."
               :appenders {}}))
