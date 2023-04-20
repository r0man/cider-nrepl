(ns cider.nrepl.middleware.log-test
  (:require [cider.log.framework :as framework]
            [cider.nrepl.test-session :as session]
            [clojure.test :refer [deftest is testing use-fixtures]]))

(use-fixtures :each session/session-fixture)

(defn frameworks []
  (vals (framework/resolve-frameworks)))

(defn uuid-str? [s]
  (try (java.util.UUID/fromString s)
       (catch Exception _)))

(defn- add-appender [framework appender & [opts]]
  (session/message (merge {:op "log-add-appender"
                           :framework (:id framework)
                           :appender appender}
                          opts)))

(defn- remove-appender [framework appender]
  (session/message {:op "log-remove-appender"
                    :framework (:id framework)
                    :appender appender}))

(deftest test-add-appender
  (doseq [framework (frameworks)]
    (let [response (add-appender framework "my-appender" {:level :debug})]
      (is (= #{"done"} (:status response)))
      (is (= {:consumers []
              :events 0
              :level "debug"
              :name "my-appender"}
             ;; TODO: Strip namespace from keyword
             (:log-add-appender response))))
    (remove-appender framework "my-appender")))

(deftest test-add-consumer
  (doseq [framework (frameworks)]
    (add-appender framework "my-appender")
    (let [response (session/message {:op "log-add-consumer"
                                     :framework (:id framework)
                                     :appender "my-appender"
                                     :consumer "my-consumer"})]
      (is (= #{"done"} (:status response)))
      (is (= {:consumers [{:name "my-consumer"}]
              :events 0
              :level []
              :name "my-appender"}
             (:add-consumer response))))
    (framework/log framework {:message "a-1"})
    ;; TODO: How to receive the log event?
    (remove-appender framework "my-appender")))

(deftest test-clear
  (doseq [framework (frameworks)]
    (add-appender framework "my-appender")
    (framework/log framework {:message "a-1"})
    (let [response (session/message {:op "log-clear-appender"
                                     :framework (:id framework)
                                     :appender "my-appender"})]
      (is (= #{"done"} (:status response)))
      (is (= {:consumers []
              :events 0
              :level []
              :name "my-appender"}
             (:clear-appender response))))))

(deftest test-exceptions
  (doseq [framework (frameworks)]
    (add-appender framework "my-appender")
    (framework/log framework {:message "a-1" :exception (IllegalArgumentException. "BOOM")})
    (framework/log framework {:message "b-1" :exception (IllegalStateException. "BOOM")})
    (framework/log framework {:message "b-2" :exception (IllegalStateException. "BOOM")})
    (let [response (session/message {:op "log-exceptions"
                                     :framework (:id framework)
                                     :appender "my-appender"})]
      (is (= #{"done"} (:status response)))
      (is (= {:java.lang.IllegalArgumentException 1
              :java.lang.IllegalStateException 2}
             (:exceptions response))))
    (remove-appender framework "my-appender")))

(deftest test-frameworks
  (doseq [framework (frameworks)]
    (let [response (session/message {:op "log-frameworks"})]
      (is (= #{"done"} (:status response)))
      (is (= {:logback
              {:appenders []
               :description "The Logback logging framework."
               :id "logback"
               :name "Logback"}}
             (:frameworks response))))
    (add-appender framework "my-appender" {:level :debug})
    (let [response (session/message {:op "log-frameworks"})]
      (is (= #{"done"} (:status response)))
      (is (= {:logback
              {:appenders [{:consumers []
                            :events 0
                            :level "debug"
                            :name "my-appender"}]
               :description "The Logback logging framework."
               :id "logback"
               :name "Logback"}}
             (:frameworks response))))
    (remove-appender framework "my-appender")))

(deftest test-inspect
  (doseq [framework (frameworks)]
    (add-appender framework "my-appender")
    (framework/log framework {:message "a-1"})
    (framework/log framework {:message "a-2"})
    (doseq [event (:search (session/message {:op "log-search"
                                             :framework (:id framework)
                                             :appender "my-appender"}))]
      (let [response (session/message {:op "log-inspect"
                                       :framework (:id framework)
                                       :appender "my-appender"
                                       :event-id (:id event)})]
        (is (= #{"done"} (:status response)))
        (is (re-matches (re-pattern (format "(?s).*%s.*" (:id event)))
                        (first (:value response))))))
    (remove-appender framework "my-appender")))

(deftest test-levels
  (doseq [framework (frameworks)]
    (add-appender framework "my-appender")
    (framework/log framework {:level :info :message "a-1"})
    (framework/log framework {:level :info :message "b-1"})
    (framework/log framework {:level :debug :message "b-2"})
    (let [response (session/message {:op "log-levels"
                                     :framework (:id framework)
                                     :appender "my-appender"})]
      (is (= #{"done"} (:status response)))
      (is (= {:debug 1 :info 2} (:levels response))))
    (remove-appender framework "my-appender")))

(deftest test-loggers
  (doseq [framework (frameworks)]
    (add-appender framework "my-appender")
    (framework/log framework {:logger "LOGGER-A" :message "a-1"})
    (framework/log framework {:logger "LOGGER-B" :message "b-1"})
    (framework/log framework {:logger "LOGGER-B" :message "b-2"})
    (let [response (session/message {:op "log-loggers"
                                     :framework (:id framework)
                                     :appender "my-appender"})]
      (is (= #{"done"} (:status response)))
      (is (= {:LOGGER-A 1 :LOGGER-B 2} (:loggers response))))
    (remove-appender framework "my-appender")))

(deftest test-search-by-level
  (doseq [framework (frameworks)]
    (add-appender framework "my-appender")
    (framework/log framework {:logger "LOGGER-A" :level :info :message "a-1"})
    (framework/log framework {:logger "LOGGER-A" :level  :warn :message "a-2"})
    (framework/log framework {:logger "LOGGER-A" :level :error :message "a-3"
                              :exception (IllegalArgumentException. "BOOM")})
    (framework/log framework {:logger "LOGGER-B" :level :trace :message "b-1"})
    (framework/log framework {:logger "LOGGER-B" :level :debug :message "b-2"})
    (framework/log framework {:logger "LOGGER-B" :level :error :message "b-3"
                              :exception (IllegalArgumentException. "BOOM")})
    (let [response (session/message {:op "log-search"
                                     :framework (:id framework)
                                     :appender "my-appender"
                                     :levels [:info :debug]})]
      (is (= #{"done"} (:status response)))
      (let [events (:search response)]
        (is (= 2 (count events)))
        (let [event (first events)]
          (is (uuid-str? (:id event)))
          (is (= "debug" (:level event)))
          (is (= "LOGGER-B" (:logger event)))
          (is (= "b-2" (:message event)))
          (is (int? (:timestamp event))))
        (let [event (second events)]
          (is (uuid-str? (:id event)))
          (is (= "info" (:level event)))
          (is (= "LOGGER-A" (:logger event)))
          (is (= "a-1" (:message event)))
          (is (int? (:timestamp event))))))
    (remove-appender framework "my-appender")))

(deftest test-search-by-exception
  (doseq [framework (frameworks)]
    (add-appender framework "my-appender")
    (framework/log framework {:logger "LOGGER-A" :level :info :message "a-1"})
    (framework/log framework {:logger "LOGGER-A" :level  :warn :message "a-2"})
    (framework/log framework {:logger "LOGGER-A" :level :error :message "a-3"
                              :exception (IllegalArgumentException. "BOOM")})
    (framework/log framework {:logger "LOGGER-B" :level :trace :message "b-1"})
    (framework/log framework {:logger "LOGGER-B" :level :debug :message "b-2"})
    (framework/log framework {:logger "LOGGER-B" :level :error :message "b-3"
                              :exception (IllegalStateException. "BOOM")})
    (let [response (session/message {:op "log-search"
                                     :framework (:id framework)
                                     :appender "my-appender"
                                     :exceptions ["java.lang.IllegalStateException"]})]
      (let [events (:search response)]
        (is (= 1 (count events)))
        (let [event (first events)]
          (is (uuid-str? (:id event)))
          (is (= "error" (:level event)))
          (is (= "LOGGER-B" (:logger event)))
          (is (= "b-3" (:message event)))
          (is (int? (:timestamp event))))))
    (remove-appender framework "my-appender")))

(deftest test-search-by-pattern
  (doseq [framework (frameworks)]
    (add-appender framework "my-appender")
    (framework/log framework {:logger "LOGGER-A" :level :info :message "a-1"})
    (framework/log framework {:logger "LOGGER-A" :level  :warn :message "a-2"})
    (framework/log framework {:logger "LOGGER-A" :level :error :message "a-3"
                              :exception (IllegalArgumentException. "BOOM")})
    (framework/log framework {:logger "LOGGER-B" :level :trace :message "b-1"})
    (framework/log framework {:logger "LOGGER-B" :level :debug :message "b-2"})
    (framework/log framework {:logger "LOGGER-B" :level :error :message "b-3"
                              :exception (IllegalStateException. "BOOM")})
    (let [response (session/message {:op "log-search"
                                     :framework (:id framework)
                                     :appender "my-appender"
                                     :pattern "b-3"})]
      (is (= #{"done"} (:status response)))
      (let [events (:search response)]
        (is (= 1 (count events)))
        (let [event (first events)]
          (is (uuid-str? (:id event)))
          (is (= "error" (:level event)))
          (is (= "LOGGER-B" (:logger event)))
          (is (= "b-3" (:message event)))
          (is (int? (:timestamp event))))))
    (remove-appender framework "my-appender")))

(deftest test-search-by-start-and-end-time
  (doseq [framework (frameworks)]
    (add-appender framework "my-appender")
    (framework/log framework {:message "a-1"})
    (Thread/sleep 100)
    (framework/log framework {:message "a-2"})
    (Thread/sleep 100)
    (framework/log framework {:message "a-3"})
    (let [response (session/message {:op "log-search"
                                     :framework (:id framework)
                                     :appender "my-appender"})]
      (is (= #{"done"} (:status response)))
      (let [[event-3 event-2 event-1]
            (:search (session/message {:op "log-search"
                                       :framework (:id framework)
                                       :appender "my-appender"}))]
        (let [response (session/message {:op "log-search"
                                         :framework (:id framework)
                                         :appender "my-appender"
                                         :start-time (inc (:timestamp event-1))
                                         :end-time (dec (:timestamp event-3))})]
          (is (= #{"done"} (:status response)))
          (let [events (:search response)]
            (is (= 1 (count events)))
            (let [event (first events)]
              (is (= (:id event-2) (:id event)))
              (is (= "info" (:level event)))
              (is (= "ROOT" (:logger event)))
              (is (= "a-2" (:message event)))
              (is (int? (:timestamp event))))))))
    (remove-appender framework "my-appender")))

(deftest test-threads
  (doseq [framework (frameworks)]
    (add-appender framework "my-appender")
    (framework/log framework {:message "a-1"})
    (let [response (session/message {:op "log-threads"
                                     :framework (:id framework)
                                     :appender "my-appender"})]
      (is (= #{"done"} (:status response)))
      (is (every? keyword? (keys (:threads response))))
      (is (every? pos-int? (vals (:threads response)))))
    (remove-appender framework "my-appender")))

(deftest test-remove-appender
  (doseq [framework (frameworks)]
    (add-appender framework "my-appender")
    (let [response (remove-appender framework "my-appender")]
      (is (= #{"done"} (:status response)))
      (is (= {:consumers []
              :events 0
              :level []
              :name "my-appender"}
             (:log-remove-appender response))))))

(deftest test-remove-consumer
  (doseq [framework (frameworks)]
    (add-appender framework "my-appender")
    (session/message {:op "log-add-consumer"
                      :framework (:id framework)
                      :appender "my-appender"
                      :consumer "my-consumer"})
    (let [response (session/message {:op "log-remove-consumer"
                                     :framework (:id framework)
                                     :appender "my-appender"
                                     :consumer "my-consumer"})]
      (is (= #{"done"} (:status response)))
      (is (= {:consumers []
              :events 0
              :level []
              :name "my-appender"}
             (:remove-consumer response))))
    (remove-appender framework "my-appender")))
