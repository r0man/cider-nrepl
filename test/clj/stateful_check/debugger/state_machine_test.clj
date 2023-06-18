(ns stateful-check.debugger.state-machine-test
  (:require [clojure.spec.test.alpha :as stest]
            [clojure.test :refer [deftest is]]
            [stateful-check.debugger.core :as debugger]
            [stateful-check.debugger.state-machine :as state-machine]
            [stateful-check.debugger.test :as test]))

(stest/instrument)

(def debugger
  (debugger/scan (debugger/debugger)))

(defn- step [state-machine transition]
  (let [state-machine' (state-machine/update-next-state state-machine transition)]
    (clojure.pprint/pprint state-machine')
    (println)
    state-machine'))

(deftest test-make-specification
  (let [debugger (debugger/scan debugger)
        debugger (debugger/run-specification debugger test/records-spec-id test/records-spec-options)
        results (debugger/last-results debugger)]
    (is (= {:state "init"
            :definition
            {"3" {:fail "3", :pass "4", :stop "cleanup"},
             "cleanup" {:pass :final},
             "4" {:fail "4", :pass #{"1b" "1a"}, :stop "cleanup"},
             "init" {:start "setup"},
             #{"1b" "1a"} {:fail #{"1b" "1a"}, :pass #{"2b"}, :stop "cleanup"},
             "setup" {:pass "initial-state"},
             #{"2b"} {:execute "cleanup"},
             "initial-state" {:pass "1"},
             "1" {:fail "1", :pass "2", :stop "cleanup"},
             "2" {:fail "2", :pass "3", :stop "cleanup"}}}
           (state-machine/make-state-machine results)))))

(deftest test-get-next-state
  (let [debugger (debugger/scan debugger)
        debugger (debugger/run-specification debugger test/records-spec-id test/records-spec-options)
        results (debugger/last-results debugger)
        state-machine (state-machine/make-state-machine results)]
    (is (= "setup" (state-machine/get-next-state state-machine :start)))
    (is (= "initial-state" (-> state-machine
                               (state-machine/update-next-state :start)
                               (state-machine/get-next-state :pass))))
    (is (= "1" (-> state-machine
                   (state-machine/update-next-state :start)
                   (state-machine/update-next-state :pass)
                   (state-machine/get-next-state :pass))))))

(deftest test-update-next-state
  (let [debugger (debugger/scan debugger)
        debugger (debugger/run-specification debugger test/records-spec-id test/records-spec-options)
        results (debugger/last-results debugger)
        state-machine (state-machine/make-state-machine results)]
    (is (= "setup" (-> state-machine
                       (state-machine/update-next-state :start)
                       :state)))))
