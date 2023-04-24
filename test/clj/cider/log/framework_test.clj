(ns cider.log.framework-test
  (:require [cider.log.appender :as appender]
            [cider.log.framework :as framework]
            [cider.log.framework.java :as java]
            [cider.log.framework.logback :as logback]
            [clojure.test :refer [deftest is testing]]))

(def appender
  {:id "my-appender"})

(defn frameworks []
  [;; (log4j2/framework)
   (logback/framework)
   (java/framework)])

(deftest test-add-appender
  (doseq [framework (frameworks)]
    (let [framework (framework/add-appender framework appender)]
      (is (framework/appender framework (:id appender)))
      (framework/remove-appender framework appender))))

(deftest test-remove-appender
  (doseq [framework (frameworks)]
    (let [framework (-> (framework/add-appender framework appender)
                        (framework/remove-appender appender))]
      (is (nil? (framework/appender framework (:id appender)))))))

(deftest test-log-levels
  (doseq [framework (frameworks)]
    (testing (:name framework)
      (let [levels (framework/levels framework)]
        (is (every? simple-keyword? (keys levels)))
        (is (every? pos-int? (vals levels)))))))

(deftest test-log-message
  (doseq [framework (frameworks)]
    (testing (:name framework)
      (let [event {:logger "my-logger"
                   :level :info
                   :message "Hello World"}
            framework (framework/add-appender framework appender)]
        (is (nil? (framework/log framework event)))
        (let [events (appender/events (framework/appender framework (:id appender)))]
          (is (= 1 (count events)))
          (let [event (first events)]
            (is (= [] (:arguments event)))
            (is (uuid? (:id event)))
            (is (= :info (:level event)))
            (is (= "my-logger" (:logger event)))
            (is (= {} (:mdc event)))
            (is (= "Hello World" (:message event)))
            (is (string? (:thread event)))
            (is (pos-int? (:timestamp event)))))
        (framework/remove-appender framework appender)))))
