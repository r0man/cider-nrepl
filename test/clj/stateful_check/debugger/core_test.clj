(ns stateful-check.debugger.core-test
  (:require [cider.nrepl.middleware.test-stateful-check :as test-stateful-check]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [stateful-check.core :as stateful-check]
            [stateful-check.debugger.core :as debugger]
            [clojure.set :as set])
  (:import [java.util UUID]))

(stest/instrument)

(def example-id
  "cider.nrepl.middleware.test-stateful-check/throw-exception-specification")

(def example-specification
  test-stateful-check/throw-exception-specification)

(defn- run-specification [specification & [options]]
  (assoc (stateful-check/run-specification specification)
         :specification specification :options options))

(def test-report
  {:summary {:ns 1 :var 1 :test 1 :pass 0 :fail 1 :error 0}
   :results
   {'cider.nrepl.middleware.test-stateful-check
    {'exception-in-assertion-is-printed
     [{:index 0
       :ns 'cider.nrepl.middleware.test-stateful-check
       :file "core.clj"
       :type :fail
       :line 275
       :var 'exception-in-assertion-is-printed
       :expected "all executions to match specification\n"
       :stateful-check (run-specification example-specification)
       :context nil
       :actual "the above execution did not match the specification\n"
       :message "Sequential prefix: ..."}]}}
   :testing-ns 'cider.nrepl.middleware.test-stateful-check
   :gen-input nil})

(def debugger
  (debugger/debugger {:test {:report (atom test-report)}}))

(deftest test-ns-specifications
  (is (every? map? (debugger/ns-specifications))))

(deftest test-find-test-event
  (is (nil? (debugger/find-test-event debugger 'a/b)))
  (is (nil? (debugger/find-test-event debugger "a/b")))
  (let [id 'cider.nrepl.middleware.test-stateful-check/exception-in-assertion-is-printed]
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
  (is (= "" (with-out-str (debugger/print debugger))))
  (let [results (run-specification example-specification)
        debugger (debugger/analyze-results debugger results)]
    (is (s/valid? :stateful-check/debugger debugger))
    (is (not (str/blank? (with-out-str (debugger/print debugger)))))))

(deftest test-render
  (is (debugger/render debugger))
  (let [results (run-specification example-specification)
        debugger (debugger/analyze-results debugger results)]
    (is (s/valid? :stateful-check/debugger debugger))
    (is (debugger/render debugger))))

(deftest test-run-specfication
  (let [debugger (debugger/scan debugger)
        debugger (debugger/run-specification debugger example-id)]
    (is (s/valid? :stateful-check/debugger debugger))))

(deftest test-scan
  (let [debugger (debugger/scan debugger)]
    (is (set/subset?
         #{{:id
            "cider.nrepl.middleware.test-stateful-check/returning-atom-as-result-spec",
            :ns 'cider.nrepl.middleware.test-stateful-check,
            :var 'returning-atom-as-result-spec,
            :type :var}
           {:id "stateful-check.debugger.core-test/example-specification",
            :ns 'stateful-check.debugger.core-test,
            :var 'example-specification,
            :type :var}
           {:id
            "cider.nrepl.middleware.test-stateful-check/java-map-specification",
            :ns 'cider.nrepl.middleware.test-stateful-check,
            :var 'java-map-specification,
            :type :var}
           {:id "cider.nrepl.middleware.test-stateful-check/records-spec",
            :ns 'cider.nrepl.middleware.test-stateful-check,
            :var 'records-spec,
            :type :var}
           {:id
            "cider.nrepl.middleware.test-stateful-check/throw-exception-specification",
            :ns 'cider.nrepl.middleware.test-stateful-check,
            :var 'throw-exception-specification,
            :type :var}
           {:id
            "cider.nrepl.middleware.test-stateful-check/exception-in-assertion-is-printed",
            :ns 'cider.nrepl.middleware.test-stateful-check,
            :var 'exception-in-assertion-is-printed,
            :type :test}}
         (set (map #(select-keys % [:id :ns :var :type])
                   (debugger/specifications debugger)))))))

(deftest test-specification
  (is (nil? (debugger/specification debugger example-id)))
  (let [debugger (debugger/scan debugger)
        specification (debugger/specification debugger example-id)]
    (is (s/valid? :stateful-check.debugger/specification specification))
    (is (= (:commands example-specification) (:commands specification)))
    (is (= 'cider.nrepl.middleware.test-stateful-check (:ns specification)))
    (is (= 'throw-exception-specification (:var specification)))
    (is (= :var (:type specification)))
    (is (= "cider.nrepl.middleware.test-stateful-check/throw-exception-specification"
           (:id specification)))))

(deftest test-specifications
  (is (nil? (debugger/specifications debugger)))
  (let [specifications (debugger/specifications (debugger/scan debugger))]
    (is (not-empty specifications))
    (is (every? #(s/valid? :stateful-check.debugger/specification %) specifications))))
