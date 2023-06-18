(ns stateful-check.debugger.state-machine-test
  (:require [clojure.spec.test.alpha :as stest]
            [clojure.test :refer [deftest is]]
            [stateful-check.debugger.core :as debugger]
            [stateful-check.debugger.state-machine :as state-machine]
            [stateful-check.debugger.test :as test]))

(stest/instrument)

(def debugger
  (debugger/scan (debugger/debugger)))

(deftest test-make-specification
  (let [debugger (debugger/scan debugger)
        options (assoc-in test/records-spec-options [:gen :threads] 0)
        debugger (debugger/run-specification debugger test/records-spec-id options)
        results (debugger/last-results debugger)]
    (is (= {:state "init"
            :definition
            {"final" {:reset "init"}
             "init" {:start "1"}
             "1" {:fail "1" :pass "2" :stop "final"}
             "2" {:fail "2" :pass "3" :stop "final"}
             "3" {:fail "3" :pass "4" :stop "final"}
             "4" {:pass "final"}}}
           (state-machine/make-state-machine results)))))

(deftest test-get-next-state
  (let [debugger (debugger/scan debugger)
        debugger (debugger/run-specification debugger test/records-spec-id test/records-spec-options)
        results (debugger/last-results debugger)
        state-machine (state-machine/make-state-machine results)]
    (is (= "1" (state-machine/get-next-state state-machine :start)))
    (is (= "2" (-> state-machine
                   (state-machine/update-next-state :start)
                   (state-machine/get-next-state :pass))))
    (is (= "3" (-> state-machine
                   (state-machine/update-next-state :start)
                   (state-machine/update-next-state :pass)
                   (state-machine/get-next-state :pass))))))

(deftest test-update-next-state
  (let [debugger (debugger/scan debugger)
        debugger (debugger/run-specification debugger test/records-spec-id test/records-spec-options)
        results (debugger/last-results debugger)
        state-machine (state-machine/make-state-machine results)]
    (is (= "1" (-> state-machine
                   (state-machine/update-next-state :start)
                   :state)))))
