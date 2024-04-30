(ns cider.nrepl.middleware.datomic
  "Datomic NREPL middleware."
  {:author "r0man", :added "0.47.2"}
  (:require [cider.nrepl.middleware.util :as util]
            [cider.nrepl.middleware.util.error-handling :refer [with-safe-transport]]
            [clojure.edn :as edn]
            [datomic.client.api :as d]))

(defn- client-server-type
  "Extract the client server type from a message."
  [msg]
  (some-> msg :cider.datomic/client :server-type keyword))

(defn- client-storage-dir
  "Extract the client storage directory from a message."
  [msg]
  (some-> msg :cider.datomic/client :storage-dir keyword))

(defn- client-system
  "Extract the client system from a message."
  [msg]
  (some-> msg :cider.datomic/client :system))

(defn- client-spec
  "Extract the client spec from a message."
  [msg]
  {:server-type (client-server-type msg)
   :storage-dir (client-storage-dir msg)
   :system (client-system msg)})

(defn- client
  "Return the Datomic client for `msg.`"
  [msg]
  (d/client (client-spec msg)))

(defn- connect
  [{:cider.datomic/keys [db-name] :as msg}]
  (d/connect (client msg) {:db-name db-name}))

(defn- tx-data [msg]
  (some-> msg :cider.datomic/tx-data edn/read-string))

;; Create database

(defn- create-database-params
  [{:cider.datomic/keys [db-name]}]
  {:db-name db-name})

(defn create-database-sync-reply
  "Create a new database."
  [msg]
  (let [client (client msg)
        params (create-database-params msg)]
    (util/transform-value
     {:cider.datomic/create-database
      (d/create-database client params)})))

;; Delete database

(defn- delete-database-params
  [{:cider.datomic/keys [db-name]}]
  {:db-name db-name})

(defn delete-database-sync-reply
  "Delete a Datomic database."
  [msg]
  (let [client (client msg)
        params (delete-database-params msg)]
    (util/transform-value
     {:cider.datomic/delete-database
      (d/delete-database client params)})))

;; List databases.

(defn- list-databases-params
  [{:cider.datomic/keys [db-name]}]
  {})

(defn list-databases-sync-reply
  "List all databases."
  [msg]
  (let [client (client msg)
        params (list-databases-params msg)]
    (util/transform-value
     {:cider.datomic/list-databases
      (d/list-databases client params)})))

;; Transact

(defn- transact-params [msg]
  {:tx-data (tx-data msg)})

;; (connect my-msg)

(defn transact-sync-reply
  "Submit a transaction."
  [msg]
  (let [client (client msg)
        params (transact-params msg)
        connection (connect msg)
        result (d/transact connection params)]
    (util/transform-value
     {:cider.datomic/transact
      (update result :tx-data pr-str)})))

(defn handle-datomic
  "Handle Datomic operations."
  [handler msg]
  (with-safe-transport handler msg
    "cider.datomic/create-database" create-database-sync-reply
    "cider.datomic/delete-database" delete-database-sync-reply
    "cider.datomic/list-databases" list-databases-sync-reply
    "cider.datomic/transact" transact-sync-reply))
