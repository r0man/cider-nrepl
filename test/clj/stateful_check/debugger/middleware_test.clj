(ns stateful-check.debugger.middleware-test
  (:require [cider.nrepl.test-session :as session]
            [clojure.test :refer [deftest is use-fixtures]])
  (:import [java.util UUID]))

(use-fixtures :each session/session-fixture)

(defn- run-failing-test []
  (require 'cider.nrepl.middleware.test-stateful-check)
  (session/message {:op "test"
                    :ns "cider.nrepl.middleware.test-stateful-check"
                    :var "exception-in-assertion-is-printed"}))

(deftest test-stateful-check-analysis
  (let [result (session/message {:op "stateful-check/analysis"
                                 :analysis "11111111-1111-1111-1111-111111111111"})]
    (is (= #{"done" "stateful-check/analysis-not-found"} (:status result)))))

(deftest test-stateful-check-analyze-test
  (run-failing-test)
  (let [result (session/message {:op "stateful-check/analyze-test"
                                 :ns "cider.nrepl.middleware.test-stateful-check"
                                 :var "exception-in-assertion-is-printed"})]
    (is (= #{"done"} (:status result)))
    (let [analysis (:stateful-check/analyze-test result)]
      ;; TODO: Why is this only working in Cider?
      ;; (is (seq (:results report)))
      )))

(deftest test-stateful-check-analyze-no-events
  (let [result (session/message {:op "stateful-check/analyze-test"
                                 :ns "user" :var "unkown"})]
    (is (= #{"done" "stateful-check/test-not-found"}
           (:status result)))))

(deftest test-stateful-check-inspect-object-not-found
  (let [result (session/message {:op "stateful-check/inspect"})]
    (is (= #{"done" "stateful-check/object-not-found"} (:status result)))))

(deftest test-stateful-check-print-object-not-found
  (let [result (session/message {:op "stateful-check/print"})]
    (is (= #{"done" "stateful-check/object-not-found"} (:status result)))))

(deftest test-stateful-check-test-reports
  (run-failing-test)
  (let [result (session/message {:op "stateful-check/test-reports"})]
    (is (= #{"done"} (:status result)))
    (is (seq (:stateful-check/test-reports result)))))

(deftest test-stateful-check-run
  (let [result (session/message {:op "stateful-check/run"
                                 :ns "cider.nrepl.middleware.test-stateful-check"
                                 :var "throw-exception-specification"})]
    (is (= #{"done"} (:status result)))
    (is (= "false" (:pass? (:stateful-check/run result))))))

(deftest test-stateful-check-specifications
  (let [result (session/message {:op "stateful-check/specifications"})]
    (is (= #{"done"} (:status result)))
    (is (seq (:stateful-check/specifications result)))))
