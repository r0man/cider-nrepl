(ns stateful-check.debugger.core-test
  (:require [cider.nrepl.middleware.test-stateful-check :as test-stateful-check]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [stateful-check.core :refer [run-specification]]
            [stateful-check.debugger.core :as debugger])
  (:import [java.util UUID]))

(def debugger
  (debugger/debugger))

(deftest test-ns-specifications
  (is (every? map? (debugger/ns-specifications))))

(deftest test-analyze-results
  (let [results (run-specification test-stateful-check/java-map-specification)
        debugger (debugger/analyze-results debugger results)]
    (is debugger)))

(deftest test-analyze-test-event
  (let [results (run-specification test-stateful-check/java-map-specification)
        event {:ns 'user :var 'test :stateful-check results}
        debugger (debugger/analyze-test-event debugger event)]
    (is debugger)))

(deftest test-print
  (is (= "" (with-out-str (debugger/print debugger))))
  (let [results (run-specification test-stateful-check/java-map-specification)
        debugger (debugger/analyze-results debugger results)]
    (is (not (str/blank? (with-out-str (debugger/print debugger)))))))

(deftest test-render
  (is (debugger/render debugger))
  (let [results (run-specification test-stateful-check/java-map-specification)
        debugger (debugger/analyze-results debugger results)]
    (is (debugger/render debugger))))

(deftest test-run-specfication
  (let [ns 'cider.nrepl.middleware.test-stateful-check
        var 'java-map-specification ]
    (is (debugger/run-specification-var debugger ns var))))
