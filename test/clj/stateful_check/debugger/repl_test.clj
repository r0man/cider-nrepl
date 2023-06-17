(ns stateful-check.debugger.repl-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is]]
            [stateful-check.debugger.core :as debugger]
            [stateful-check.debugger.render :as render]
            [stateful-check.debugger.repl :as repl]
            [stateful-check.debugger.test :as test])
  (:import [java.util UUID]))

(def specification
  test/records-spec)

(def options
  test/records-spec-options)

(def example-id
  "stateful-check.debugger.test/records-spec")

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
    (is (string? (:id results)))))

;; (deftest test-get-command
;;   (repl/reset!)
;;   (repl/scan)
;;   (let [results (repl/run-specification example-id options)
;;         first-command (repl/get-command {:results (:id results)
;;                                          :case :first
;;                                          :handle "1a"})
;;         smallest-command (repl/get-command {:results (:id results)
;;                                             :case :smallest
;;                                             :handle "1b"})]
;;     (is (= (sv/->RootVar "1") (:handle first-command)))
;;     (is (= (sv/->RootVar "1") (:handle smallest-command)))
;;     (is (not= first-command smallest-command))))

;; (deftest test-get-command
;;   (repl/reset!)
;;   (repl/scan)
;;   (let [results (repl/run-specification example-id options)]
;;     (is (= (sv/->RootVar "1") (:handle first-command)))
;;     (is (= (sv/->RootVar "1") (:handle smallest-command)))
;;     (is (not= first-command smallest-command))))

(deftest test-scan
  (repl/reset!)
  (let [debugger (repl/scan)]
    (is (s/valid? :stateful-check/debugger debugger))))

(comment

  (repl/run-specification specification options)

  (-> (repl/run-specification examples/returning-atom-as-result-spec
                              examples/returning-atom-as-result-options)
      :shrunk :result-data)

  (-> (repl/run-specification specification options)
      :shrunk :result-data :executions)

  (-> (repl/run-specification specification options)
      render/render-results)

  (-> (repl/run-specification specification options)
      :result-data :executions)

  )
