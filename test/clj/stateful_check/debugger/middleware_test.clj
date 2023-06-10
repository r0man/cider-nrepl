(ns stateful-check.debugger.middleware-test
  (:require [cider.nrepl.test-session :as session]
            [clojure.test :refer [deftest is use-fixtures]])
  (:import [java.util UUID]))

(use-fixtures :each session/session-fixture)

(defn- run-failing-test []
  (require 'cider.nrepl.middleware.test-stateful-check)
  (session/message {:op "test"
                    :ns "cider.nrepl.middleware.test-stateful-check"
                    :var "java-map-passes-sequentially"}))

(deftest test-stateful-check-analyze
  (run-failing-test)
  (let [result (session/message {:op "stateful-check-analyze"})]
    (is (= #{"done"} (:status result)))
    (let [report (:stateful-check-analyze result)]
      ;; TODO: Why is this only working in Cider?
      ;; (is (seq (:results report)))
      )))

(deftest test-stateful-check-analyze-no-events
  (let [result (session/message {:op "stateful-check-analyze"})]
    (is (= #{"done"} (:status result)))
    (let [report (:stateful-check-analyze result)]
      (is (empty? (:results report))))))

(deftest test-stateful-check-inspect-object-not-found
  (let [result (session/message {:op "stateful-check-inspect"})]
    (is (= #{"done" "stateful-check-object-not-found"} (:status result)))))

(deftest test-stateful-check-print-object-not-found
  (let [result (session/message {:op "stateful-check-print"})]
    (is (= #{"done" "stateful-check-object-not-found"} (:status result)))))

(deftest test-stateful-check-report
  (run-failing-test)
  (session/message {:op "stateful-check-analyze"})
  (let [result (session/message {:op "stateful-check-report"})]
    (is (= #{"done"} (:status result)))
    (let [report (:stateful-check-report result)]
      ;; TODO: Why is this only working in Cider?
      ;; (is (seq (:results report)))
      )))

(deftest test-stateful-check-report-without-events
  (let [result (session/message {:op "stateful-check-report"})]
    (is (= #{"done"} (:status result)))
    (let [report (:stateful-check-report result)]
      (is (empty? (:results report))))))

(deftest test-stateful-check-report-events-not-analyzed
  (run-failing-test)
  (let [result (session/message {:op "stateful-check-report"})]
    (is (= #{"done"} (:status result)))
    (let [report (:stateful-check-report result)]
      (is (empty? (:results report))))))
