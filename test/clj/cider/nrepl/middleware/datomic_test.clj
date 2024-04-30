(ns cider.nrepl.middleware.datomic-test
  (:require [cider.nrepl.test-session :as session]
            [clojure.test :refer [deftest is use-fixtures]]
            [clojure.edn :as edn]))

(use-fixtures :each session/session-fixture)

(def movie-schema
  [{:db/ident :movie/title
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "The title of the movie"}

   {:db/ident :movie/genre
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "The genre of the movie"}

   {:db/ident :movie/release-year
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db/doc "The year the movie was released in theaters"}])

(def first-movies
  [{:movie/title "The Goonies"
    :movie/genre "action/adventure"
    :movie/release-year 1985}
   {:movie/title "Commando"
    :movie/genre "thriller/action"
    :movie/release-year 1985}
   {:movie/title "Repo Man"
    :movie/genre "punk dystopia"
    :movie/release-year 1984}])

(def all-titles-q
  '[:find ?movie-title
    :where [_ :movie/title ?movie-title]])

(def client
  {:server-type :datomic-local
   :storage-dir :mem
   :system "default"})

(defn- create-database
  [client db-name]
  (session/message {:op "cider.datomic/create-database"
                    "cider.datomic/client" client
                    "cider.datomic/db-name" db-name}))

(defn- delete-database
  [client db-name]
  (session/message {:op "cider.datomic/delete-database"
                    "cider.datomic/client" client
                    "cider.datomic/db-name" db-name}))

(defn- list-databases
  [client]
  (session/message {:op "cider.datomic/list-databases"
                    "cider.datomic/client" client}))

(defn- transact
  [client db-name tx-data]
  (session/message {:op "cider.datomic/transact"
                    "cider.datomic/client" client
                    "cider.datomic/db-name" db-name
                    "cider.datomic/tx-data" (pr-str tx-data)}))

(defn- query
  [client db-name query]
  (session/message {:op "cider.datomic/query"
                    "cider.datomic/client" client
                    "cider.datomic/db-name" db-name
                    "cider.datomic/query" (pr-str query)}))

(deftest test-create-database
  (delete-database client "foo")
  (let [response (create-database client "foo")]
    (is (= #{"done"} (:status response)))
    (is (= "true" (:cider.datomic/create-database response)))))

(deftest test-delete-database
  (delete-database client "foo")
  (let [response (delete-database client "foo")]
    (is (= #{"done"} (:status response)))
    (is (= "true" (:cider.datomic/delete-database response))))
  (create-database client "foo")
  (let [response (delete-database client "foo")]
    (is (= #{"done"} (:status response)))
    (is (= "true" (:cider.datomic/delete-database response)))))

(deftest test-list-databases
  (delete-database client "foo")
  (let [response (list-databases client)]
    (is (= #{"done"} (:status response)))
    (is (= [] (:cider.datomic/list-databases response))))
  (create-database client "foo")
  (let [response (list-databases client)]
    (is (= #{"done"} (:status response)))
    (is (= ["foo"] (:cider.datomic/list-databases response)))))

(deftest test-transact
  (create-database client "foo")
  (let [response (transact client "foo" movie-schema)]
    (is (= #{"done"} (:status response)))
    (let [{:keys [db-after db-before tx-data] :as result} (:cider.datomic/transact response)]
      (is (pos-int? (:basisT db-after)))
      (is (pos-int? (:basisT db-before)))
      (is (string? tx-data)))))

(deftest test-query
  (create-database client "foo")
  (transact client "foo" movie-schema)
  (transact client "foo" first-movies)
  (let [response (query client "foo" all-titles-q)]
    (is (= #{"done"} (:status response)))
    (let [result (:cider.datomic/query response)]
      (is (= [["Commando"]
              ["The Goonies"]
              ["Repo Man"]]
             (edn/read-string result))))))
