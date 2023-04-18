(ns cider.log.framework-test
  (:require [cider.log.appender :as appender]
            [cider.log.framework :as framework]
            [cider.log.framework.logback :as logback]
            [clojure.test :refer [deftest is testing]]))

(defn frameworks []
  [;; (log4j2/framework)
   (logback/framework)])

(deftest test-add-appender
  (doseq [framework (frameworks)]
    (let [appender {:name "my-appender"}
          framework (framework/add-appender framework appender)]
      (is (framework/appender framework (:name appender)))
      (framework/remove-appender framework appender))))

(deftest test-remove-appender
  (doseq [framework (frameworks)]
    (let [appender {:name "my-appender"}
          framework (-> (framework/add-appender framework appender)
                        (framework/remove-appender appender))]
      (is (nil? (framework/appender framework (:name appender)))))))

(deftest test-log-message
  (doseq [framework (take 1 (frameworks))]
    (testing (:name framework)
      (let [appender {:name "my-appender"}
            event {:level :info :message "Hello World"}
            framework (framework/add-appender framework appender)]
        (is (nil? (framework/log framework event)))
        (let [events (appender/events (framework/appender framework (:name appender)))]
          (is (= 1 (count events)))
          (let [event (first events)]
            (is (= [] (:data event)))
            (is (uuid? (:id event)))
            (is (= :info (:level event)))
            (is (= "ROOT" (:logger event)))
            (is (= {} (:mdc event)))
            (is (= "Hello World" (:message event)))
            (is (string? (:thread event)))
            (is (pos-int? (:timestamp event)))))
        (framework/remove-appender framework appender)))))
