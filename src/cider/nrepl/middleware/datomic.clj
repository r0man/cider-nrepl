(ns cider.nrepl.middleware.datomic
  "Datomic NREPL middleware."
  {:author "r0man"
   :added "0.47.2"}
  (:require [cider.nrepl.middleware.inspect :as middleware.inspect]
            [cider.nrepl.middleware.util.error-handling :refer [with-safe-transport]]
            [datomic.client.api :as d]
            [haystack.analyzer :as analyzer]
            [haystack.parser.clojure.throwable :as throwable]
            [logjam.event :as event]
            [logjam.framework :as framework]
            [nrepl.middleware.print :as print]
            [nrepl.misc :refer [response-for]]
            [nrepl.transport :as transport]
            [orchard.inspect :as orchard.inspect])
  (:import (java.io StringWriter)
           (java.util UUID)))

(defn- connection-spec [msg]
  {:server-type :datomic-local
   :storage-dir :mem
   :system "ci"})

(defn- client [msg]
  (d/client (connection-spec msg)))

;; Create database.

(defn- create-database-params
  [{:cider.datomic/keys [db-name]}]
  {:db-name db-name})

(defn create-database-sync-reply
  "Create a new database."
  [msg]
  (let [client (client msg)]
    {:cider.datomic/create-databases
     (d/create-database client (create-database-params msg))}))

;; List databases.

(defn- list-databases-params
  [{:cider.datomic/keys [db-name]}]
  {})

(defn list-databases-sync-reply
  "List all databases."
  [msg]
  (let [client (client msg)]
    {:cider.datomic/list-databases
     (d/list-databases client (list-databases-params msg))}))

;; (create-database-sync-reply {:cider.datomic/db-name "foo"})
;; (create-database-sync-reply {:cider.datomic/db-name "bar"})
;; (list-databases-sync-reply {})

(defn handle-datomic
  "Handle Datomic operations."
  [handler {:keys [session] :as msg}]
  ;; (when-not (contains? (meta session) ::frameworks)
  ;;   (alter-meta! session assoc ::frameworks (framework/resolve-frameworks)))
  (with-safe-transport handler msg
    "cider.datomic/create-database" create-database-sync-reply
    "cider.datomic/list-databases" list-databases-sync-reply))
