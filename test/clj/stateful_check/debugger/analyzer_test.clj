(ns stateful-check.debugger.analyzer-test
  (:require [cider.nrepl.middleware.test-stateful-check :as test-stateful-check]
            [clojure.test :refer [deftest is testing]]
            [stateful-check.core :refer [run-specification]]
            [stateful-check.debugger.analyzer :as analyzer]
            [stateful-check.debugger.cursor :as cursor]))

(def specification
  test-stateful-check/java-map-specification)

(def options
  {:gen {:threads 2} :run {:seed 0}})

(defn- sequential-arguments [analysis path]
  (->> (get-in analysis path) :sequential (mapcat :arguments)))

(defn- sequential-results [analysis path]
  (->> (get-in analysis path) :sequential (map :result)))

(defn- parallel-arguments [analysis path]
  (->> (get-in analysis path) :parallel (mapcat identity) (mapcat :arguments)))

(defn- parallel-results [analysis path]
  (->> (get-in analysis path) :parallel (mapcat identity) (map :result)))

(defn- check-analysis [specification options results analysis]
  (is (string? (:id analysis)))
  (is (= specification (:specification analysis)))
  (is (= results (:results analysis)) results)
  (is (= options (:options analysis)) results)
  (doseq [executions [[:executions :first]
                      [:executions :smallest]]]
    (testing "sequential execution"
      (testing "arguments have been indexed for inspection"
        (doseq [argument (sequential-arguments analysis executions)]
          (is (= (:value argument)
                 (get-in analysis (:path argument))
                 (get-in analysis (cursor/parse (:cursor argument))))))))
    (testing "results have been indexed for inspection"
      (doseq [result (sequential-results analysis executions)]
        (is (= (:value result)
               (get-in analysis (:path result))
               (get-in analysis (cursor/parse (:cursor result)))))))
    (testing "parallel execution"
      (testing "arguments have been indexed for inspection"
        (doseq [argument (parallel-arguments analysis executions)]
          (is (= (:value argument)
                 (get-in analysis (:path argument))
                 (get-in analysis (cursor/parse (:cursor argument))))))))
    (testing "results have been indexed for inspection"
      (doseq [result (parallel-results analysis executions)]
        (is (= (:value result)
               (get-in analysis (:path result))
               (get-in analysis (cursor/parse (:cursor result)))))))))

(deftest test-analyze-quick-check
  (let [results (run-specification specification options)
        analysis (analyzer/analyze-quick-check (analyzer/analyzer) specification options results)]
    (check-analysis specification options results analysis)))

(deftest test-analyze-test-report-event
  (let [results (run-specification specification options)
        test-event {:ns 'user
                    :var 'test
                    :stateful-check
                    {:specification specification
                     :options options
                     :results results}}
        analysis (analyzer/analyze-test-report-event (analyzer/analyzer) test-event)]
    (check-analysis specification options results analysis)))
