(ns stateful-check.debugger.repl-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is]]
            [stateful-check.debugger.core :as debugger]
            [stateful-check.debugger.repl :as repl]
            [stateful-check.debugger.test :as test])
  (:import [java.util UUID]))

(def specification
  test/records-failure-spec)

(def options
  test/records-spec-options)

(def example-id
  "stateful-check.debugger.test/records-failure-spec")

(deftest test-reset!
  (is (= (debugger/debugger) (repl/reset!))))

(deftest test-get-results
  (repl/reset!)
  (repl/scan)
  (is (nil? (repl/get-results "n/a")))
  (let [results (repl/run-specification example-id options)]
    (is (= results (repl/get-results (:id results))))
    (is (= results (repl/get-results {:results (:id results)})))))

(deftest test-last-results
  (repl/reset!)
  (repl/scan)
  (is (nil? (repl/last-results)))
  (let [results (repl/run-specification example-id options)]
    (is (= results (repl/last-results)))))

(deftest test-run-specification
  (repl/reset!)
  (repl/scan)
  (let [results (repl/run-specification example-id options)]
    (is (string? (:id results))))
  (clojure.pprint/pprint
   (-> (repl/last-results)
       :result-data
       ;; (select-keys [:environments])
       :environments)))

(deftest test-evaluate-step
  (repl/reset!)
  (repl/scan)
  (repl/run-specification example-id options)
  (clojure.pprint/pprint
   (-> (repl/evaluate-step :case :first)
       :result-data
       (select-keys [:evaluations
                     :state-machine])))

  (clojure.pprint/pprint
   (-> (repl/evaluate-step :case :first)
       :result-data
       (select-keys [:evaluations
                     :state-machine])))

  ;; (clojure.pprint/pprint
  ;;  (-> (repl/evaluate-step :case :first)
  ;;      :result-data
  ;;      (select-keys [:evaluations
  ;;                    :state-machine])))
  ;; (clojure.pprint/pprint
  ;;  (-> (repl/evaluate-step :case :first)
  ;;      :result-data
  ;;      (select-keys [:evaluations
  ;;                    :state-machine])))
  ;; (clojure.pprint/pprint
  ;;  (-> (repl/evaluate-step :case :first)
  ;;      :result-data
  ;;      (select-keys [:evaluations
  ;;                    :state-machine])))
  ;; (clojure.pprint/pprint
  ;;  (-> (repl/evaluate-step :case :first)
  ;;      :result-data
  ;;      (select-keys [:evaluations
  ;;                    :state-machine])))
  )

(deftest test-scan
  (repl/reset!)
  (let [debugger (repl/scan)]
    (is (s/valid? :stateful-check/debugger debugger))))
