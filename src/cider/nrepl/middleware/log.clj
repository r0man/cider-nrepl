(ns cider.nrepl.middleware.log
  (:require [cider.log.appender :as appender]
            [cider.log.event :as event]
            [cider.log.framework :as framework]
            [cider.nrepl.middleware.inspect :as middleware.inspect]
            [cider.nrepl.middleware.util.error-handling :refer [with-safe-transport]]
            [nrepl.misc :refer [response-for]]
            [nrepl.transport :as transport]
            [orchard.inspect :as orchard.inspect])
  (:import [java.util UUID]))

(defn- select-consumer
  "Return the log `consumer` in a Bencode compatible format."
  [consumer]
  (-> (select-keys consumer [:id :filters])
      (update :id str)))

(defn- select-appender
  "Return the log `appender` in a Bencode compatible format."
  [appender]
  {:consumers (map select-consumer (appender/consumers appender))
   :events (count (appender/events appender))
   :filters (appender/filters appender)
   :id (appender/id appender)
   :size (appender/size appender)
   :threshold (appender/threshold appender)})

(defn- select-framework
  "Return the log `frameowrk` in a Bencode compatible format."
  [framework]
  {:appenders (map select-appender (framework/appenders framework))
   :id (framework/id framework)
   :javadoc-url (framework/javadoc-url framework)
   :name (framework/name framework)
   :website-url (framework/website-url framework)})

(defn- select-event
  "Return the log `event` in a Bencode compatible format."
  [{:keys [arguments id] :as event}]
  (cond-> (select-keys event [:level :logger :message :id :thread :timestamp])
    (uuid? id)
    (update :id str)
    ;; TODO: Get rid of this
    (map? (first arguments))
    (assoc :message (pr-str (dissoc (first arguments) :context)))))

;; TODO: Double check this this! Sometimes inspecting a log event works only
;; after inspecting something else with the Cider inspector.
(defn- inspect-value
  "Show `value` in the Cider inspector"
  [{:keys [page-size max-atom-length max-coll-size] :as msg} value]
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
  (or (get-in (meta session) [::frameworks framework])
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
  "Swap the framework bound in the session by applying `f` with `args`."
  [msg f & args]
  (if-let [framework (framework msg)]
    (-> (:session msg)
        (alter-meta! update-in [::frameworks (:id framework)] #(apply f % args))
        (get-in [::frameworks (:id framework)]))
    (throw (ex-info "Log framework not found"
                    {:type :logging-framework-not-found
                     :framework (:framework msg)}))))

(defn add-appender-reply
  "Add an appender to a log framework."
  [{:keys [appender filters size threshold] :as msg}]
  {:log-add-appender
   (-> (swap-framework!
        msg framework/add-appender
        (cond-> {:id appender}
          (map? filters)
          (assoc :filters filters)
          (pos-int? size)
          (assoc :size size)
          (nat-int? threshold)
          (assoc :threshold threshold)))
       (framework/appender-by-id appender)
       (select-appender))})

(defn add-consumer-reply
  "Add a consumer to an appender of a log framework."
  [{:keys [filters transport] :as msg}]
  (let [consumer {:id (UUID/randomUUID)
                  :filters (or filters {})
                  :filter-fn (event/search-filter filters)
                  :callback (fn [consumer event]
                              (->> (response-for msg
                                                 :log-consumer (str (:id consumer))
                                                 :log-event (select-event event)
                                                 :status :log-event)
                                   (transport/send transport)))}
        appender (appender/add-consumer (appender msg) consumer)]
    {:log-add-consumer (select-consumer (appender/consumer-by-id appender (:id consumer)))}))

(defn clear-appender-reply
  "Clear all events of a log appender."
  [msg]
  {:log-clear-appender (select-appender (appender/clear (appender msg)))})

(defn exceptions-reply
  "Return the exceptions and their frequencies for the given framework and appender."
  [msg]
  {:log-exceptions (-> msg appender appender/events event/exception-frequencies)})

(defn frameworks-reply
  "Return the available log frameworks."
  [{:keys [session]}]
  {:log-frameworks (->> (get (meta session) ::frameworks) vals (map select-framework))})

(defn inspect-event-reply
  "Inspect a log event."
  [{:keys [event-id] :as msg}]
  (inspect-value msg (appender/event (appender msg) (UUID/fromString event-id))))

(defn levels-reply
  "Return the log levels and their frequencies for the given framework and appender."
  [msg]
  {:log-levels (-> msg appender appender/events event/level-frequencies)})

(defn loggers-reply
  "Return the loggers and their frequencies for the given framework and appender."
  [msg]
  {:log-loggers (-> msg appender appender/events event/logger-frequencies)})

(defn remove-appender-reply
  "Remove an appender from a log framework."
  [msg]
  (let [appender (appender msg)]
    (swap-framework! msg framework/remove-appender {:id (:appender msg)})
    {:log-remove-appender (select-appender appender)}))

(defn remove-consumer-reply
  "Remove a consumer from the appender of a log framework."
  [msg]
  (let [consumer (consumer msg)]
    (appender/remove-consumer (appender msg) consumer)
    {:log-remove-consumer (select-consumer consumer)}))

(defn update-appender-reply
  "Update the appender of a log framework."
  [{:keys [filters size threshold] :as msg}]
  (let [appender (appender msg)]
    {:log-update-appender
     (-> (swap-framework!
          msg framework/update-appender
          (cond-> {:id (appender/id appender)}
            (map? filters)
            (assoc :filters filters)
            (pos-int? size)
            (assoc :size size)
            (nat-int? threshold)
            (assoc :threshold threshold)))
         (framework/appender (appender/id appender))
         (select-appender))}))

(defn update-consumer-reply
  "Update the consumer of a log appender."
  [{:keys [filters] :as msg}]
  (let [consumer (consumer msg)
        appender (appender/update-consumer
                  (appender msg) consumer
                  #(assoc % :filters filters :filter-fn (event/search-filter filters)))]
    {:log-update-consumer (select-consumer (appender/consumer-by-id appender (:id consumer)))}))

(defn search-reply
  "Search the log events of an appender."
  [{:keys [filters limit] :as msg}]
  {:log-search (->> (cond-> {}
                      (map? filters)
                      (assoc :filters filters)
                      (nat-int? limit)
                      (assoc :limit limit))
                    (event/search (appender/events (appender msg)))
                    (map select-event))})

(defn threads-reply
  "Return the threads and their frequencies for the given framework and appender."
  [msg]
  {:log-threads (-> msg appender appender/events event/thread-frequencies)})

(defn handle-log
  "Handle NREPL log operations."
  [handler {:keys [session] :as msg}]
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
    "log-update-appender" update-appender-reply
    "log-update-consumer" update-consumer-reply
    "log-search" search-reply
    "log-threads" threads-reply))
