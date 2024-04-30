(ns cider.nrepl.middleware.datomic
  "Datomic NREPL middleware."
  {:author "r0man", :added "0.47.2"}
  (:require [cider.nrepl.middleware.util :as util]
            [cider.nrepl.middleware.util.error-handling :refer [with-safe-transport]]
            [datomic.client.api :as d]))

(defn- connection-spec [msg]
  {:server-type :datomic-local
   :storage-dir :mem
   :system "ci"})

(defn- client [msg]
  (d/client (connection-spec msg)))

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
    ;; (prn "LIST")
    ;; (clojure.pprint/pprint (dissoc msg :session))
    (util/transform-value
     {:cider.datomic/list-databases
      (d/list-databases client params)})))

(defn handle-datomic
  "Handle Datomic operations."
  [handler msg]
  (with-safe-transport handler msg
    "cider.datomic/create-database" create-database-sync-reply
    "cider.datomic/delete-database" delete-database-sync-reply
    "cider.datomic/list-databases" list-databases-sync-reply))
