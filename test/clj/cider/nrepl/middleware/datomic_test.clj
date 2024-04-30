(ns cider.nrepl.middleware.datomic-test
  (:require [cider.nrepl.test-session :as session]
            [clojure.test :refer [deftest is use-fixtures]]))

(use-fixtures :each session/session-fixture)

(def client
  {:server-type :datomic-local
   :storage-dir :mem
   :system "ci"})

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
    (clojure.pprint/pprint response)
    (is (= #{"done"} (:status response)))
    (is (= [] (:cider.datomic/list-databases response))))
  (create-database client "foo")
  (let [response (list-databases client)]
    (is (= #{"done"} (:status response)))
    (is (= ["foo"] (:cider.datomic/list-databases response)))))
