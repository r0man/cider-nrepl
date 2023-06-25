(ns stateful-check.debugger.repl-test
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is testing]]
            [stateful-check.debugger.core :as debugger]
            [stateful-check.debugger.repl :as repl]
            [stateful-check.debugger.test :as test]
            [stateful-check.symbolic-values :as sv])
  (:import [java.util UUID]))

(def spec-id
  "stateful-check.debugger.test/records-failure-spec")

(def spec-options
  test/records-spec-options)

(deftest test-clear
  (let [debugger (repl/clear)]
    (is (s/valid? :stateful-check/debugger debugger))
    (is (= (debugger/debugger) debugger))))

(deftest test-get-env
  (repl/reset)
  (is (nil? (repl/get-env)))
  (repl/run-specification spec-id spec-options)
  (let [env (repl/get-env :case :first :handle "1")]
    (is (= (sv/->RootVar "1") (:handle env))))
  (let [env (repl/get-env :handle "2")]
    (is (= (sv/->RootVar "2") (:handle env)))))

(deftest test-get-run
  (repl/reset)
  (is (nil? (repl/get-run)))
  (let [run-1 (repl/run-specification spec-id spec-options)]
    (is (s/valid? :stateful-check.debugger/run run-1))
    (is (= run-1 (repl/get-run)))
    (is (= run-1 (repl/get-run :id (:id run-1))))
    (is (= run-1 (repl/get-run :run (:id run-1))))
    (let [run-2 (repl/run-specification spec-id spec-options)]
      (is (not= run-1 run-2))
      (is (s/valid? :stateful-check.debugger/run run-2))
      (is (= run-2 (repl/get-run)))
      (is (= run-2 (repl/get-run :id (:id run-2))))
      (is (= run-2 (repl/get-run :run (:id run-2)))))))

(deftest test-run-specification
  (repl/reset)
  (let [run (repl/run-specification spec-id spec-options)]
    (is (s/valid? :stateful-check.debugger/run run))
    (testing "first failing case"
      (testing "arguments"
        (is (= [{:index 0, :real -3, :symbolic -3, :name "0"}]
               (repl/get-arguments :case :first :handle "1")))
        (is (= [{:index 0, :real 2, :symbolic 2, :name "0"}]
               (repl/get-arguments :case :first :handle "2")))
        (is (= [{:index 0, :real -2, :symbolic -2, :name "0"}]
               (repl/get-arguments :case :first :handle "3")))
        (is (= [{:index 0, :real -4, :symbolic -4, :name "0"}]
               (repl/get-arguments :case :first :handle "4")))
        (is (= [{:index 0, :real "id--4", :symbolic (get (sv/->RootVar "4") :id), :name "id"}]
               (repl/get-arguments :case :first :handle "5"))))
      (testing "bindings"
        (is (= {:real
                {(sv/->RootVar "setup") {}
                 (sv/->RootVar "1") {:id "id--3", :value -3}},
                :symbolic
                {(sv/->RootVar "setup") (sv/->RootVar "setup")
                 (sv/->RootVar "1") (sv/->RootVar "1")}}
               (repl/get-bindings :case :first :handle "1")))
        (is (= {:real
                {(sv/->RootVar "setup") {},
                 (sv/->RootVar "1") {:id "id--3", :value -3},
                 (sv/->RootVar "2") {:id "id-2", :value 2}},
                :symbolic
                {(sv/->RootVar "setup") (sv/->RootVar "setup")
                 (sv/->RootVar "1") (sv/->RootVar "1")
                 (sv/->RootVar "2") (sv/->RootVar "2")}}
               (repl/get-bindings :case :first :handle "2")))
        (is (= {:real
                {(sv/->RootVar "setup") {},
                 (sv/->RootVar "1") {:id "id--3", :value -3},
                 (sv/->RootVar "2") {:id "id-2", :value 2},
                 (sv/->RootVar "3") {:id "id--2", :value "boom"}},
                :symbolic
                {(sv/->RootVar "setup") (sv/->RootVar "setup")
                 (sv/->RootVar "1") (sv/->RootVar "1")
                 (sv/->RootVar "2") (sv/->RootVar "2")
                 (sv/->RootVar "3") (sv/->RootVar "3")}}
               (repl/get-bindings :case :first :handle "3")))
        (is (= {:real
                {(sv/->RootVar "setup") {},
                 (sv/->RootVar "1") {:id "id--3", :value -3},
                 (sv/->RootVar "2") {:id "id-2", :value 2},
                 (sv/->RootVar "3") {:id "id--2", :value "boom"},
                 (sv/->RootVar "4") {:id "id--4", :value "boom"}},
                :symbolic
                {(sv/->RootVar "setup") (sv/->RootVar "setup")
                 (sv/->RootVar "1") (sv/->RootVar "1")
                 (sv/->RootVar "2") (sv/->RootVar "2")
                 (sv/->RootVar "3") (sv/->RootVar "3")
                 (sv/->RootVar "4") (sv/->RootVar "4")}}
               (repl/get-bindings :case :first :handle "4")))
        (is (= {:real
                {(sv/->RootVar "setup") {},
                 (sv/->RootVar "1") {:id "id--3", :value -3},
                 (sv/->RootVar "2") {:id "id-2", :value 2},
                 (sv/->RootVar "3") {:id "id--2", :value "boom"},
                 (sv/->RootVar "4") {:id "id--4", :value "boom"},
                 (sv/->RootVar "5") {:id "id--4", :value "boom"}},
                :symbolic
                {(sv/->RootVar "setup") (sv/->RootVar "setup"),
                 (sv/->RootVar "1") (sv/->RootVar "1"),
                 (sv/->RootVar "2") (sv/->RootVar "2"),
                 (sv/->RootVar "3") (sv/->RootVar "3"),
                 (sv/->RootVar "4") (sv/->RootVar "4"),
                 (sv/->RootVar "5") (sv/->RootVar "5")}}
               (repl/get-bindings :case :first :handle "5")))))
    (testing "smallest failing case"
      (testing "arguments"
        (is (= [{:index 0, :real 0, :symbolic 0, :name "0"}]
               (repl/get-arguments :case :smallest :handle "2")))
        (is (= [{:index 0, :real -1, :symbolic -1, :name "0"}]
               (repl/get-arguments :case :smallest :handle "3")))
        (is (= [{:index 0, :real 0, :symbolic 0, :name "0"}]
               (repl/get-arguments :case :smallest :handle "4")))
        (is (= [{:index 0, :real "id-0", :symbolic (get (sv/->RootVar "2") :id), :name "id"}]
               (repl/get-arguments :case :smallest :handle "5")))))))

(deftest test-evaluate-step
  (repl/reset)
  (repl/run-specification spec-id spec-options)

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

(deftest test-reset
  (let [debugger (repl/reset)]
    (is (s/valid? :stateful-check/debugger debugger))))

(deftest test-scan
  (repl/clear)
  (is (nil? (repl/specifications)))
  (let [specifications (repl/scan)]
    (is (seq specifications))
    (is (every? #(s/valid? :stateful-check.debugger/specification %) specifications))
    (is (= specifications (repl/specifications)))))

(deftest test-specification
  (repl/reset)
  (let [id "stateful-check.debugger.test/records-failure-spec"
        specification (repl/specification id)]
    (is (s/valid? :stateful-check/specification specification))
    (is {:id "stateful-check.debugger.test/records-failure-spec"
         :ns 'stateful-check.debugger.test
         :type :var
         :var 'records-failure-spec}
        (select-keys specification [:id :ns :var :type]))))

(deftest test-specifications
  (repl/reset)
  (let [specifications (repl/specifications)]
    (is (seq specifications))
    (is (every? #(s/valid? :stateful-check/specification %) specifications))
    (is (set/subset?
         #{{:id "stateful-check.debugger.test/records-error-spec"
            :ns 'stateful-check.debugger.test
            :type :var
            :var 'records-error-spec}
           {:id "stateful-check.debugger.test/records-failure-spec"
            :ns 'stateful-check.debugger.test
            :type :var
            :var 'records-failure-spec}}
         (set (map #(select-keys % [:id :ns :var :type]) specifications))))))
