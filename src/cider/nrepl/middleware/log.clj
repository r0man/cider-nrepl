(ns cider.nrepl.middleware.log
  (:require [cider.nrepl.middleware.inspect :as middleware.inspect]
            [cider.nrepl.middleware.util.error-handling :refer [with-safe-transport]]
            [nrepl.middleware :as middleware :refer [set-descriptor!]]
            [nrepl.middleware.session :as middleware.session]
            [nrepl.misc :refer [response-for]]
            [nrepl.transport :as transport]
            [orchard.inspect :as orchard.inspect]
            [cider.log.appender :as appender]
            [cider.log.event :as event]
            [cider.log.framework :as framework])
  (:import [java.util UUID]))

(defn- select-consumer [consumer]
  (select-keys consumer [:name]))

(defn- select-appender [appender]
  {:consumers (map select-consumer (appender/consumers appender))
   :events (count (appender/events appender))
   :level (appender/level appender)
   :name (appender/name appender)})

(defn- select-framework [framework]
  (-> (select-keys framework [:id :name :description])
      (assoc :appenders (map select-appender (framework/appenders framework)))))

(defn- to-wire [{:keys [data id] :as record}]
  (cond-> (select-keys record [:level :logger :message :id :thread :timestamp])
    (uuid? id)
    (update :id str)
    (map? (first data))
    (assoc :message (pr-str (dissoc (first data) :context)))))

(defn- inspect-value [{:keys [page-size max-atom-length max-coll-size] :as msg} value]
  (let [inspector (middleware.inspect/swap-inspector!
                   msg #(-> (assoc % :page-size (or page-size 32)
                                   :indentation 0
                                   :max-atom-length max-atom-length
                                   :max-coll-size max-coll-size)
                            (orchard.inspect/start value)))]
    (#'middleware.inspect/inspector-response msg inspector)))

(defn- framework
  "Lookup the framework from the `msg`."
  [{:keys [session framework]}]
  (or (get-in (meta session) [::frameworks (keyword framework)])
      (throw (ex-info "Logging framework not found"
                      {:error :logging-framework-not-found
                       :framework framework}))))

(defn- appender
  "Lookup the appender from the `msg`."
  [{:keys [appender] :as msg}]
  (or (some-> msg framework (framework/appender appender))
      (throw (ex-info "Logging appender not found"
                      {:error :logging-appender-not-found
                       :framework (:framework msg)
                       :appender appender}))))

(defn swap-framework!
  [msg f & args]
  (if-let [framework (framework msg)]
    (-> (:session msg)
        (alter-meta! update-in [::frameworks (:id framework)] #(apply f % args))
        (get-in [::frameworks (:id framework)]))
    (throw (ex-info "Logging framework not found"
                    {:type :logging-framework-not-found
                     :framework (:framework msg)}))))

(defn response [msg result]
  (let [statuses (:status result)]
    (cond
      (and (set? statuses)
           (contains? statuses :error))
      result
      (= :error statuses)
      result
      :else
      {(:op msg) result})))

(defn add-appender-reply
  [{:keys [appender level] :as msg}]
  (let [framework (swap-framework! msg framework/add-appender {:name appender :level level})]
    (response msg (select-appender (framework/appender framework appender)))))

(defn add-consumer-reply
  [{:keys [consumer level transport] :as msg}]
  (let [consumer {:name consumer
                  :level level
                  :callback (fn [event]
                              (->> (response-for msg :status :log-event :event (to-wire event))
                                   (transport/send transport)))}]
    {:add-consumer (select-appender (appender/add-consumer (appender msg) consumer))}))

(defn clear-appender-reply
  [msg]
  {:clear-appender (select-appender (appender/clear (appender msg)))})

(defn inspect-reply
  [{:keys [event-id] :as msg}]
  (inspect-value msg (appender/event (appender msg) (UUID/fromString event-id))))

(defn exceptions-reply [msg]
  {:exceptions (event/exception-frequencies (appender/events (appender msg)))})

(defn frameworks-reply [{:keys [session]}]
  (let [frameworks (vals (get (meta session) ::frameworks))]
    {:frameworks
     (zipmap (map :id frameworks)
             (map select-framework frameworks))}))

(defn levels-reply [msg]
  {:levels (event/level-frequencies (appender/events (appender msg)))})

(defn loggers-reply [msg]
  {:loggers (event/logger-frequencies (appender/events (appender msg)))})

(defn remove-consumer-reply
  [{:keys [consumer] :as msg}]
  {:remove-consumer (select-appender (appender/remove-consumer (appender msg) {:name consumer}))})

(defn remove-appender-reply [msg]
  (let [appender (appender msg)]
    (swap-framework! msg framework/remove-appender {:name (:appender msg)})
    (response msg (select-appender appender))))

(defn search-reply
  [{:keys [end-time exceptions levels loggers limit pattern start-time threads] :as msg}]
  {:search (->> (event/search (appender/events (appender msg))
                              {:end-time end-time
                               :exceptions exceptions
                               :levels levels
                               :limit limit
                               :loggers loggers
                               :pattern pattern
                               :start-time start-time
                               :threads threads})
                (map to-wire))})

(defn threads-reply [msg]
  {:threads (event/thread-frequencies (appender/events (appender msg)))})

(defn wrap-log [handler]
  (fn [{:keys [session] :as msg}]
    (when-not (contains? (meta session) ::frameworks)
      (alter-meta! session assoc ::frameworks (framework/resolve-frameworks)))
    (with-safe-transport handler msg
      "stem.log/add-appender" add-appender-reply
      "stem.log/add-consumer" add-consumer-reply
      "stem.log/clear-appender" clear-appender-reply
      "stem.log/exceptions" exceptions-reply
      "stem.log/frameworks" frameworks-reply
      "stem.log/inspect" inspect-reply
      "stem.log/levels" levels-reply
      "stem.log/loggers" loggers-reply
      "stem.log/remove-appender" remove-appender-reply
      "stem.log/remove-consumer" remove-consumer-reply
      "stem.log/search" search-reply
      "stem.log/threads" threads-reply)))

(def descriptor
  {:requires #{#'middleware.session/session}
   :handles
   {"stem.log/add-appender"
    {:doc "Add an appender to a logging framework."
     :requires {"framework" "The id of the logging framework."
                "appender" "The name of the appender."
                "level" "The log level used by the appender."}
     :returns {"status" "done"
               "add-appender" "The appender that was added."}}

    "stem.log/add-consumer"
    {:doc "Add a consumer to an appender of a logging framework."
     :requires {"framework" "The id of the logging framework."
                "appender" "The name of the appender."
                "consumer" "The name of the consumer."
                "level" "The log level used by the consumer."}
     :returns {"status" "done"
               "add-consumer" "The consumer that was added."}}

    "stem.log/clear-appender"
    {:doc "Clear all events of an appender."
     :requires {"framework" "The id of the logging framework."
                "appender" "The name of the appender."}
     :returns {"status" "done"
               "clear-appender" "The appender that was cleared."}}

    "stem.log/exceptions"
    {:doc "Return the exceptions and their frequencies for the given framework and appender."
     :requires {"framework" "The id of the logging framework."
                "appender" "The name of the appender."}
     :returns {"status" "done"
               "exceptions" "A map from exception name to event frequency."}}

    "stem.log/frameworks"
    {:doc "Return the available logging frameworks."
     :returns {"status" "done"
               "frameworks" "The available logging frameworks indexed by id."}}

    "stem.log/inspect"
    {:doc "Inspect a log event."
     :requires {"framework" "The id of the logging framework."
                "appender" "The name of the appender."
                "event-id" "The id of the event to inspect."}
     :returns {"status" "done"
               "value" "The inspection result."}}

    "stem.log/levels"
    {:doc "Return the log levels and their frequencies for the given framework and appender."
     :requires {"framework" "The id of the logging framework."
                "appender" "The name of the appender."}
     :returns {"status" "done"
               "levels" "A map from log level to event frequency."}}

    "stem.log/loggers"
    {:doc "Return the loggers and their frequencies for the given framework and appender."
     :requires {"framework" "The id of the logging framework."
                "appender" "The name of the appender."}
     :returns {"status" "done"
               "loggers" "A map from logger name to event frequency."}}

    "stem.log/remove-consumer"
    {:doc "Remove a consumer from an appender of a logging framework."
     :requires {"framework" "The id of the logging framework."
                "appender" "The name of the appender."
                "consumer" "The name of the consumer."}
     :returns {"status" "done"
               "add-consumer" "The removed consumer."}}

    "stem.log/remove-appender"
    {:doc "Remove an appender from a logging framework."
     :requires {"framework" "The id of the logging framework."
                "appender" "The name of the appender."}
     :returns {"status" "done"
               "remove-appender" "The removed appender."}}

    "stem.log/search"
    {:doc "Search the log events of an appender."
     :requires {"framework" "The id of the logging framework."
                "appender" "The name of the appender."}
     :optional {"end-time" "Filter events with a timestamp less than end time."
                "exceptions" "The list of exception names used to filter records."
                "levels" "The list of log levels used to filter records."
                "limit" "Number of records to return."
                "loggers" "The list of logger names used to filter records."
                "pattern" "The regular expression used to filter records."
                "start-time" "Filter events with a timestamp greater than start time."
                "threads" "The list of thread names used to filter records."}
     :returns {"status" "done"
               "search" "The list of log events matching the search."}}

    "stem.log/threads"
    {:doc "Return the threads and their frequencies for the given framework and appender."
     :requires {"framework" "The id of the logging framework."
                "appender" "The name of the appender."}
     :returns {"status" "done"
               "threads" "A map from thread name to event frequency."}}}})

(set-descriptor! #'wrap-log descriptor)
