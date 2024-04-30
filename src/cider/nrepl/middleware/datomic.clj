(ns cider.nrepl.middleware.datomic
  "Datomic NREPL middleware."
  {:author "r0man", :added "0.47.2"}
  (:require [cider.nrepl.middleware.util :as util]
            [cider.nrepl.middleware.util.error-handling :refer [with-safe-transport]]
            [clojure.edn :as edn]
            [datomic.client.api :as d]))

(defn- client-params
  "Extract the client spec from a message."
  [{:cider.datomic/keys [client]}]
  (let [{:keys [endpoint server-type storage-dir system region]} client]
    (cond-> {:server-type (keyword server-type)
             :storage-dir (keyword storage-dir)
             :system system}
      endpoint (assoc :endpoint endpoint)
      region (assoc :region region))))

(defn- client
  "Return the Datomic client for `msg.`"
  [msg]
  (d/client (client-params msg)))

(defn- connect
  [{:cider.datomic/keys [db-name] :as msg}]
  (d/connect (client msg) {:db-name db-name}))

(defn- db [msg]
  (d/db (connect msg)))

(defn- tx-data
  "Extract the transaction data from `msg`."
  [msg]
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

;; Query

(defn- query-params [msg]
  (some-> msg :cider.datomic/query edn/read-string))

(defn query-sync-reply
  "Submit a queryion."
  [msg]
  (let [client (client msg)
        result (d/q (query-params msg) (db msg))]
    (util/transform-value
     {:cider.datomic/query
      (pr-str result)})))

(defn handle-datomic
  "Handle Datomic operations."
  [handler msg]
  (with-safe-transport handler msg
    "cider.datomic/create-database" create-database-sync-reply
    "cider.datomic/delete-database" delete-database-sync-reply
    "cider.datomic/list-databases" list-databases-sync-reply
    "cider.datomic/transact" transact-sync-reply
    "cider.datomic/query" query-sync-reply))
