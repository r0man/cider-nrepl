(ns cider.nrepl.middleware.datomic-test
  (:require [cider.nrepl.test-session :as session]
            [clojure.set :as set]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check.generators :as gen]
            [logjam.framework :as framework]))

(use-fixtures :each session/session-fixture)

(deftest test-list-databases
  (let [response (session/message {:op "cider.datomic/list-databases"})]
    (is (= #{"done"} (:status response)))
    (is (= []
           (:cider.datomic/list-databases response)))))
