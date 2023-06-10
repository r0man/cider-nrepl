(ns stateful-check.debugger.analyzer-test
  (:require [cider.nrepl.middleware.test-stateful-check :as test-stateful-check]
            [clojure.test :refer [deftest is testing]]
            [stateful-check.core :refer [run-specification]]
            [stateful-check.debugger.analyzer :as analyzer])
  (:import [java.util UUID]))

;; (def specification
;;   test-stateful-check/java-map-specification)

;; (def options
;;   {:gen {:threads 2} :run {:seed 0}})

;; (defn- sequential-arguments [analysis path]
;;   (->> (get-in analysis path) :sequential (mapcat :arguments)))

;; (defn- sequential-results [analysis path]
;;   (->> (get-in analysis path) :sequential (map :result)))

;; (defn- parallel-arguments [analysis path]
;;   (->> (get-in analysis path) :parallel (mapcat identity) (mapcat :arguments)))

;; (defn- parallel-results [analysis path]
;;   (->> (get-in analysis path) :parallel (mapcat identity) (map :result)))

;; (defn- check-analysis [analysis]
;;   (doseq [path [[:result-data :executions :first-failing]
;;                 [:result-data :executions :smallest-failing]]]
;;     (testing "sequential execution"
;;       (testing "arguments have been indexed for inspection"
;;         (doseq [argument (sequential-arguments analysis path)]
;;           (is (= (:value argument)
;;                  (get-in analysis (:path argument))
;;                  (get-in analysis (cursor/parse (:cursor argument))))))))
;;     (testing "results have been indexed for inspection"
;;       (doseq [result (sequential-results analysis path)]
;;         (is (= (:value result)
;;                (get-in analysis (:path result))
;;                (get-in analysis (cursor/parse (:cursor result)))))))
;;     (testing "parallel execution"
;;       (testing "arguments have been indexed for inspection"
;;         (doseq [argument (parallel-arguments analysis path)]
;;           (is (= (:value argument)
;;                  (get-in analysis (:path argument))
;;                  (get-in analysis (cursor/parse (:cursor argument))))))))
;;     (testing "results have been indexed for inspection"
;;       (doseq [result (parallel-results analysis path)]
;;         (is (= (:value result)
;;                (get-in analysis (:path result))
;;                (get-in analysis (cursor/parse (:cursor result)))))))))

;; (deftest test-analyze-quick-check
;;   (let [specification test-stateful-check/java-map-specification
;;         results (run-specification specification options)
;;         analysis (analyzer/analyze-results (analyzer/analyzer) results)]
;;     (check-analysis analysis)))

;; (deftest test-analyze-quick-check-exception
;;   (let [specification test-stateful-check/throw-exception-specification
;;         results (run-specification specification options)
;;         analysis (analyzer/analyze-results (analyzer/analyzer) results)]
;;     (check-analysis analysis)))

;; (deftest test-analyze-test-report-event
;;   (let [specification test-stateful-check/java-map-specification
;;         results (run-specification specification options)
;;         event {:ns 'user :var 'test :stateful-check results}
;;         analysis (analyzer/analyze-test-event (analyzer/analyzer) event)]
;;     (is (= 'user (:ns (:test analysis))))
;;     (is (= 'test (:var (:test analysis))))
;;     (is (= event (:event (:test analysis))))
;;     (check-analysis analysis)))
