(ns stateful-check.debugger.core-test
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [stateful-check.core :as stateful-check]
            [stateful-check.debugger.core :as debugger]
            [stateful-check.debugger.test :as test])
  (:import [java.util UUID]))

(stest/instrument)

(def example-id
  "stateful-check.debugger.test/records-spec")

(def example-specification
  test/records-spec)

(defn- run-specification [specification & [options]]
  (assoc (stateful-check/run-specification specification)
         :specification specification :options options))

(def test-report
  {:summary {:ns 1 :var 1 :test 1 :pass 0 :fail 1 :error 0}
   :results
   {'stateful-check.debugger.test
    {'test-records
     [{:index 0
       :ns 'stateful-check.debugger.test
       :file "core.clj"
       :type :fail
       :line 275
       :var 'test-records
       :expected "all executions to match specification\n"
       :stateful-check (run-specification example-specification)
       :context nil
       :actual "the above execution did not match the specification\n"
       :message "Sequential prefix: ..."}]}}})

(def debugger
  (debugger/debugger {:test {:report (atom test-report)}}))

(deftest test-ns-specifications
  (is (every? map? (debugger/ns-specifications))))

(deftest test-find-test-event
  (is (nil? (debugger/find-test-event debugger 'a/b)))
  (is (nil? (debugger/find-test-event debugger "a/b")))
  (let [id 'stateful-check.debugger.test/test-records]
    (is (debugger/find-test-event debugger id))
    (is (debugger/find-test-event debugger (str id)))))

(deftest test-analyze-results
  (let [results (run-specification example-specification)
        debugger (debugger/analyze-results debugger results)]
    (is (s/valid? :stateful-check/debugger debugger))))

(deftest test-analyze-test-event
  (let [results (run-specification example-specification)
        event {:ns 'user :var 'test :stateful-check results}
        debugger (debugger/analyze-test-event debugger event)]
    (is (s/valid? :stateful-check/debugger debugger))))

(deftest test-last-results
  (is (nil? (debugger/last-results debugger)))
  (let [debugger (debugger/scan debugger)]
    (is (nil? (debugger/last-results debugger)))
    (let [debugger (debugger/run-specification debugger example-id)]
      (is (debugger/last-results debugger)))))

(deftest test-print
  (let [debugger (debugger/scan debugger)
        debugger (debugger/run-specification debugger example-id)
        run (:id (debugger/last-results debugger))]
    (is (string? (with-out-str (debugger/print debugger run))))))

(deftest test-run-specfication
  (let [debugger (debugger/scan debugger)
        debugger (debugger/run-specification debugger example-id)]
    (is (s/valid? :stateful-check/debugger debugger))))

(deftest test-scan
  (let [debugger (debugger/scan debugger)]
    (is (set/subset?
         #{{:id "stateful-check.debugger.test/records-spec"
            :ns 'stateful-check.debugger.test
            :type :var
            :var 'records-spec}
           {:ns 'stateful-check.debugger.test
            :var 'test-records
            :id "stateful-check.debugger.test/test-records"
            :type :test}}
         (set (map #(select-keys % [:id :ns :var :type])
                   (debugger/specifications debugger)))))))

(deftest test-specification
  (is (nil? (debugger/specification debugger example-id)))
  (let [debugger (debugger/scan debugger)
        specification (debugger/specification debugger example-id)]
    (is (s/valid? :stateful-check.debugger/specification specification))
    (is (= (:commands example-specification) (:commands specification)))
    (is (= 'stateful-check.debugger.test (:ns specification)))
    (is (= 'records-spec (:var specification)))
    (is (= :var (:type specification)))
    (is (= "stateful-check.debugger.test/records-spec"
           (:id specification)))))

(deftest test-specifications
  (is (nil? (debugger/specifications debugger)))
  (let [specifications (debugger/specifications (debugger/scan debugger))]
    (is (not-empty specifications))
    (is (every? #(s/valid? :stateful-check.debugger/specification %) specifications))))
