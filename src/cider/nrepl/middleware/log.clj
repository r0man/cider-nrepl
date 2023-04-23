(ns cider.nrepl.middleware.log
  (:require [cider.nrepl.middleware.inspect :as middleware.inspect]
            [cider.nrepl.middleware.util.error-handling :refer [with-safe-transport]]
            [nrepl.misc :refer [response-for]]
            [nrepl.transport :as transport]
            [orchard.inspect :as orchard.inspect]
            [cider.log.appender :as appender]
            [cider.log.event :as event]
            [cider.log.framework :as framework])
  (:import [java.util UUID]))

(defn- select-consumer [consumer]
  (-> (select-keys consumer [:id :filter])
      (update :id str)))

(defn- select-appender [appender]
  {:consumers (map select-consumer (appender/consumers appender))
   :events (count (appender/events appender))
   :level (appender/level appender)
   :name (appender/name appender)})

(defn- select-framework [framework]
  {:appenders (map select-appender (framework/appenders framework))
   :description (framework/description framework)
   :id (framework/id framework)
   :javadoc-url (framework/javadoc-url framework)
   :name (framework/name framework)
   :website-url (framework/website-url framework)})

(defn- to-wire [{:keys [arguments id] :as record}]
  (cond-> (select-keys record [:level :logger :message :id :thread :timestamp])
    (uuid? id)
    (update :id str)
    (map? (first arguments))
    (assoc :message (pr-str (dissoc (first arguments) :context)))))

(defn- inspect-value [{:keys [page-size max-atom-length max-coll-size] :as msg} value]
  (let [inspector (middleware.inspect/swap-inspector!
                   msg #(-> (assoc % :page-size (or page-size 32)
                                   :indentation 0
                                   :max-atom-length max-atom-length
                                   :max-coll-size max-coll-size)
                            (orchard.inspect/start value)))]
    (#'middleware.inspect/inspector-response msg inspector)))

(defn- framework
  "Lookup the log framework from the :framework key of `msg`."
  [{:keys [session framework]}]
  (or (get-in (meta session) [::frameworks (keyword framework)])
      (throw (ex-info "Log framework not found"
                      {:error :logging-framework-not-found
                       :framework framework}))))

(defn- appender
  "Lookup the log appender from the :framework and :appender keys of `msg`."
  [{:keys [appender] :as msg}]
  (or (some-> msg framework (framework/appender appender))
      (throw (ex-info "Log appender not found"
                      {:error :logging-appender-not-found
                       :framework (:framework msg)
                       :appender appender}))))

(defn- consumer
  "Lookup the log appender from the :framework, :appender and :consumer keys of `msg`."
  [{:keys [consumer] :as msg}]
  (or (appender/consumer-by-id (appender msg) (UUID/fromString consumer))
      (throw (ex-info "Log consumer not found"
                      {:error :logging-consumer-not-found
                       :framework (:framework msg)
                       :appender (:appender msg)
                       :consumer consumer}))))

(defn swap-framework!
  [msg f & args]
  (if-let [framework (framework msg)]
    (-> (:session msg)
        (alter-meta! update-in [::frameworks (:id framework)] #(apply f % args))
        (get-in [::frameworks (:id framework)]))
    (throw (ex-info "Log framework not found"
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
  [{:keys [filter transport] :as msg}]
  (let [consumer {:id (UUID/randomUUID)
                  :filter (or filter {})
                  :callback (fn [event]
                              (->> (response-for msg :status :log-event :event (to-wire event))
                                   (transport/send transport)))}
        appender (appender/add-consumer (appender msg) consumer)]
    {:log-add-consumer (select-consumer (appender/consumer-by-id appender (:id consumer)))}))

(defn clear-appender-reply
  [msg]
  {:log-clear-appender (select-appender (appender/clear (appender msg)))})

(defn inspect-event-reply
  [{:keys [event-id] :as msg}]
  (inspect-value msg (appender/event (appender msg) (UUID/fromString event-id))))

(defn exceptions-reply [msg]
  {:log-exceptions (event/exception-frequencies (appender/events (appender msg)))})

(defn frameworks-reply [{:keys [session]}]
  (let [frameworks (vals (get (meta session) ::frameworks))]
    {:log-frameworks
     (zipmap (map :id frameworks)
             (map select-framework frameworks))}))

(defn levels-reply [msg]
  {:log-levels (event/level-frequencies (appender/events (appender msg)))})

(defn loggers-reply [msg]
  {:log-loggers (event/logger-frequencies (appender/events (appender msg)))})

(defn remove-appender-reply [msg]
  (let [appender (appender msg)]
    (swap-framework! msg framework/remove-appender {:name (:appender msg)})
    (response msg (select-appender appender))))

(defn remove-consumer-reply [msg]
  (let [consumer (consumer msg)]
    (appender/remove-consumer (appender msg) (:id consumer))
    {:log-remove-consumer (select-consumer consumer)}))

(defn update-consumer-reply [msg]
  (let [consumer (consumer msg)
        appender (appender/update-consumer
                  (appender msg) consumer
                  #(merge % (select-keys msg [:filter])))]
    {:log-update-consumer (select-consumer (appender/consumer-by-id appender (:id consumer)))}))

(defn search-reply
  [{:keys [filter limit] :as msg}]
  {:log-search (->> (event/search (appender/events (appender msg))
                                  {:filter filter :limit limit})
                    (map to-wire))})

(defn threads-reply [msg]
  {:log-threads (event/thread-frequencies (appender/events (appender msg)))})

(defn handle-log [handler {:keys [session] :as msg}]
  (when-not (contains? (meta session) ::frameworks)
    (alter-meta! session assoc ::frameworks (framework/resolve-frameworks)))
  (with-safe-transport handler msg
    "log-add-appender" add-appender-reply
    "log-add-consumer" add-consumer-reply
    "log-clear-appender" clear-appender-reply
    "log-exceptions" exceptions-reply
    "log-frameworks" frameworks-reply
    "log-inspect-event" inspect-event-reply
    "log-levels" levels-reply
    "log-loggers" loggers-reply
    "log-remove-appender" remove-appender-reply
    "log-remove-consumer" remove-consumer-reply
    "log-update-consumer" update-consumer-reply
    "log-search" search-reply
    "log-threads" threads-reply))
