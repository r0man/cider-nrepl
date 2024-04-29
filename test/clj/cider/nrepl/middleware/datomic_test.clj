(ns cider.nrepl.middleware.datomic-test
  (:require [cider.nrepl.test-session :as session]
            [clojure.set :as set]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check.generators :as gen]
            [logjam.framework :as framework]))

(use-fixtures :each session/session-fixture)

(def datomic-local-mem
  {:server-type :datomic-local
   :storage-dir :mem
   :system "ci"})

(deftest test-create-database
  (let [response (session/message {:op "cider.datomic/create-database"
                                   "cider.datomic/client" datomic-local-mem
                                   "cider.datomic/db-name" "foo"})]
    (is (= #{"done"} (:status response)))
    (is (= "true"
           (:cider.datomic/create-database response)))))

(deftest test-list-databases
  (let [response (session/message {:op "cider.datomic/list-databases"
                                   "cider.datomic/client" datomic-local-mem})]
    (is (= #{"done"} (:status response)))
    (is (= []
           (:cider.datomic/list-databases response)))))
