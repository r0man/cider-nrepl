(ns stateful-check.debugger.test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [stateful-check.core :refer [specification-correct?]]))

(def records-spec-id
  "stateful-check.debugger.test/records-spec")

(def records-test-id
  "stateful-check.debugger.test/test-records")

(def records (atom {}))

(defn store-record-failure [value]
  (let [id (str "id-" value)
        value (if (>= (count @records) 3) "boom" value)]
    (swap! records assoc id {:id id :value value})
    {:id id :value value}))

(defn store-record-error [value]
  (let [id (str "id-" value)]
    (if (>= (count @records) 3)
      (throw (ex-info "BOOM" {:id id :value value})))
    (swap! records assoc id {:id id :value value})
    {:id id :value value}))

(defn retrieve-record [id]
  (get @records id))

(def store-record-spec
  {:args (fn [_] [gen/int])
   :command store-record-error
   :next-state (fn [state [value] record]
                 (assoc state (:id record)
                        {:id (:id record)
                         :value value}))})

(def retrieve-record-spec
  {:requires seq
   :args (fn [state]
           [(gen/elements (keys state))])
   :command #'retrieve-record
   :postcondition (fn [state _ [id] value]
                    (is (= (get state id) value)))})

(def records-spec
  {:commands {:store #'store-record-spec
              :retrieve #'retrieve-record-spec}
   :setup #(reset! records {})
   :initial-state (constantly {})
   :cleanup (fn [_] (reset! records {}))})

(def records-spec-options
  {:gen {:threads 2}
   :report {:first-case? true}
   :run {:seed 100}})

;; This test is expected to fail. Set :min-clj-version very high so this test is
;; skipped by the existing :test-selector machinery.
(deftest ^{:min-clj-version "9.9.9"} test-records
  (is (specification-correct? records-spec records-spec-options)))
