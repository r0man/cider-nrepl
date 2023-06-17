(ns stateful-check.debugger.eval-test
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer [deftest is]]
            [stateful-check.debugger.core :as debugger]
            [stateful-check.debugger.test :as test]
            [stateful-check.debugger.eval :as eval]))

(stest/instrument)

(def debugger
  (debugger/debugger))

(deftest test-run-specfication
  (let [debugger (debugger/scan debugger)
        debugger (debugger/run-specification debugger test/records-spec-id test/records-spec-options)
        results (debugger/last-results debugger)]
    (def my-results results)
    (clojure.pprint/pprint (eval/make-state-machine (:specification results) results))))
