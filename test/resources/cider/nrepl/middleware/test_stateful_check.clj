(ns cider.nrepl.middleware.test-stateful-check
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [stateful-check.core :refer :all]))

;; Java Map

(def ^java.util.Map system-under-test (java.util.TreeMap.))

(def test-keys ["" "a" "house" "tree" "λ"])

(defn map-put [key value]
  (.put system-under-test key (if (> value 42) "boom" value)))

(defn map-get [key]
  (.get system-under-test key))

(def put-command
  {:args (fn [state] [(gen/elements test-keys) gen/int])
   :command #'map-put
   :next-state (fn [state [k v] _]
                 (assoc state k v))})

(def get-command
  {:requires (fn [state] (seq state))
   :args (fn [state] [(gen/elements test-keys)])
   :command #'map-get
   :postcondition (fn [prev-state _ [k] val]
                    (is (= (get prev-state k) val)))})

(def java-map-specification
  {:commands {:put #'put-command
              :get #'get-command}
   :setup #(.clear system-under-test)})

(deftest java-map-passes-sequentially
  (is (specification-correct? java-map-specification
                              {:gen {:threads 2}
                               :report {:command-frequency? true
                                        :first-case? true
                                        :stacktrace? true}
                               :run {:seed 0}})))

;; Store record

(def records (atom {}))

(defn store-record [value]
  (let [id (gensym "id")
        value (if (> value 42) "boom" value)]
    (swap! records assoc id {:id id :value value})
    {:id id :value value}))

(defn retrieve-record [id]
  (get @records id))

(def store-record-spec
  {:args (fn [_] [gen/int])
   :command #'store-record
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
              :retrieve #'retrieve-record-spec}})

(def records-spec-options
  {:gen {:threads 2}
   :report {:first-case? true}
   :run {:seed 100}})

(deftest test-store-record-spec
  (is (specification-correct? records-spec records-spec-options)))

(def throw-exception-specification
  {:commands {:cmd {:command (constantly true)
                    :postcondition (fn [_ _ _ _]
                                     (is (throw (ex-info "An exception!" {:oh "no"}))))}}})

;; (deftest exception-in-assertion-is-printed
;;   (is (throw (ex-info "An exception!" {:oh "no"})))
;;   (is (specification-correct? throw-exception-specification)))
