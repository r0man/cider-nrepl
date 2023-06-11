(ns stateful-check.debugger.repl-test
  (:require [cider.nrepl.middleware.test-stateful-check :as examples]
            [clojure.test :refer [deftest is]]
            [stateful-check.debugger.core :as debugger]
            [stateful-check.debugger.render :as render]
            [stateful-check.debugger.repl :as repl]
            [stateful-check.symbolic-values :as sv])
  (:import [java.util UUID]))

(def specification
  examples/records-spec)

(def options
  examples/records-spec-options)

(deftest test-reset!
  (is (= (debugger/debugger) (repl/reset!))))

(deftest test-get-analysis
  (repl/reset!)
  (is (nil? (repl/get-analysis (UUID/randomUUID))))
  (let [analysis (repl/run-specification specification options)]
    (is (= analysis (repl/get-analysis (:id analysis))))
    (is (= analysis (repl/get-analysis {:analysis (:id analysis)})))))

(deftest test-last-analysis
  (repl/reset!)
  (is (nil? (repl/last-analysis)))
  (let [analysis (repl/run-specification specification options)]
    (is (= analysis (repl/last-analysis)))))

(deftest test-run-specification
  (repl/reset!)
  (let [analysis (repl/run-specification specification options)]
    (is (uuid? (:id analysis)))
    (is (= specification (-> analysis :result-data :specification)))))

(deftest test-get-command
  (repl/reset!)
  (let [analysis (repl/run-specification specification options)
        first-command (repl/get-command {:analysis (:id analysis)
                                         :case :first
                                         :handle "1"})
        smallest-command (repl/get-command {:analysis (:id analysis)
                                            :case :smallest
                                            :handle "1"})]
    (is (= (sv/->RootVar "1") (:handle first-command)))
    (is (= (sv/->RootVar "1") (:handle smallest-command)))
    (is (not= first-command smallest-command))))

(comment

  (repl/run-specification specification options)

  (-> (repl/run-specification examples/returning-atom-as-result-spec
                              examples/returning-atom-as-result-options)
      :shrunk :result-data)

  (-> (repl/run-specification specification options)
      :shrunk :result-data :executions)

  (-> (repl/run-specification specification options)
      render/render-analysis)

  (-> (repl/run-specification specification options)
      :result-data :executions)

  )
