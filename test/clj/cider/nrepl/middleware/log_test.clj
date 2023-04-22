(ns cider.nrepl.middleware.log-test
  (:require [cider.log.framework :as framework]
            [cider.log.specs]
            [cider.nrepl.test-session :as session]
            [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check.generators :as gen]
            [cider.log.event :as event]))

(use-fixtures :each session/session-fixture)

(def appender-name
  "my-appender")

(defn frameworks []
  (vals (framework/resolve-frameworks)))

(defn- level-gen [framework]
  (gen/elements (keys (framework/levels framework))))

(defn- event-gen [framework]
  (gen/fmap (fn [[event level]]
              (assoc event :level level))
            (gen/tuple (s/gen :cider.log/event)
                       (level-gen framework))))

(defn- uuid-str? [s]
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
    (let [response (add-appender framework appender-name {:level :info})]
      (is (= #{"done"} (:status response)))
      (is (= {:consumers []
              :events 0
              :level "info"
              :name appender-name}
             (:log-add-appender response))))
    (remove-appender framework appender-name)))

(deftest test-add-consumer
  (doseq [framework (frameworks)]
    (add-appender framework appender-name)
    (let [response (session/message {:op "log-add-consumer"
                                     :framework (:id framework)
                                     :appender appender-name
                                     :consumer "my-consumer"})]
      (is (= #{"done"} (:status response)))
      (is (= {:consumers [{:name "my-consumer"}]
              :events 0
              :level []
              :name appender-name}
             (:add-consumer response))))
    (framework/log framework {:message "a-1"})
    ;; TODO: How to receive the log event?
    (remove-appender framework appender-name)))

(deftest test-clear
  (doseq [framework (frameworks)]
    (add-appender framework appender-name)
    (framework/log framework {:message "a-1"})
    (let [response (session/message {:op "log-clear-appender"
                                     :framework (:id framework)
                                     :appender appender-name})]
      (is (= #{"done"} (:status response)))
      (is (= {:consumers []
              :events 0
              :level []
              :name appender-name}
             (:clear-appender response))))))

(deftest test-exceptions
  (doseq [framework (frameworks)]
    (add-appender framework appender-name)
    (framework/log framework {:message "a-1" :exception (IllegalArgumentException. "BOOM")})
    (framework/log framework {:message "b-1" :exception (IllegalStateException. "BOOM")})
    (framework/log framework {:message "b-2" :exception (IllegalStateException. "BOOM")})
    (let [response (session/message {:op "log-exceptions"
                                     :framework (:id framework)
                                     :appender appender-name})]
      (is (= #{"done"} (:status response)))
      (is (= {:java.lang.IllegalArgumentException 1
              :java.lang.IllegalStateException 2}
             (:exceptions response))))
    (remove-appender framework appender-name)))

(deftest test-frameworks
  (let [response (session/message {:op "log-frameworks"})]
    (doseq [framework (frameworks)]
      (is (= #{"done"} (:status response)))
      (is (= {:appenders []
              :description (framework/description framework)
              :id (name (framework/id framework))
              :javadoc-url (framework/javadoc-url framework)
              :name (framework/name framework)
              :website-url (framework/website-url framework)}
             (get-in response [:frameworks (framework/id framework)]))))))

(deftest test-frameworks-add-appender
  (doseq [framework (frameworks)]
    (add-appender framework appender-name {:level :info})
    (let [response (session/message {:op "log-frameworks"})]
      (is (= #{"done"} (:status response)))
      (is (= {:appenders [{:consumers []
                           :events 0
                           :level "info"
                           :name appender-name}]
              :description (framework/description framework)
              :id (name (framework/id framework))
              :javadoc-url (framework/javadoc-url framework)
              :name (framework/name framework)
              :website-url (framework/website-url framework)}
             (get-in response [:frameworks (framework/id framework)]))))
    (remove-appender framework appender-name)))

(deftest test-inspect
  (doseq [framework (frameworks)]
    (add-appender framework appender-name)
    (framework/log framework {:message "a-1"})
    (framework/log framework {:message "a-2"})
    (doseq [event (:search (session/message {:op "log-search"
                                             :framework (:id framework)
                                             :appender appender-name}))]
      (let [response (session/message {:op "log-inspect-event"
                                       :framework (:id framework)
                                       :appender appender-name
                                       :event-id (:id event)})]
        (is (= #{"done"} (:status response)))
        (is (re-matches (re-pattern (format "(?s).*%s.*" (:id event)))
                        (first (:value response))))))
    (remove-appender framework appender-name)))

(deftest test-levels
  (doseq [framework (frameworks)
          :let [[level-1 level-2] (reverse (keys (framework/levels framework)))]]
    (add-appender framework appender-name)
    (framework/log framework {:level level-1 :message "a-1"})
    (framework/log framework {:level level-1 :message "b-1"})
    (framework/log framework {:level level-2 :message "b-2"})
    (let [response (session/message {:op "log-levels"
                                     :framework (:id framework)
                                     :appender appender-name})]
      (is (= #{"done"} (:status response)))
      (is (= {level-2 1 level-1 2} (:levels response))))
    (remove-appender framework appender-name)))

(deftest test-loggers
  (doseq [framework (frameworks)]
    (add-appender framework appender-name)
    (framework/log framework {:logger "LOGGER-A" :message "a-1"})
    (framework/log framework {:logger "LOGGER-B" :message "b-1"})
    (framework/log framework {:logger "LOGGER-B" :message "b-2"})
    (let [response (session/message {:op "log-loggers"
                                     :framework (:id framework)
                                     :appender appender-name})]
      (is (= #{"done"} (:status response)))
      (is (= {:LOGGER-A 1 :LOGGER-B 2} (:loggers response))))
    (remove-appender framework appender-name)))

(deftest test-search-by-level
  (doseq [framework (frameworks)
          :let [[level-1 level-2 level-3]
                (reverse (keys (framework/levels framework)))]]
    (add-appender framework appender-name)
    (framework/log framework {:level level-1 :message "a-1"})
    (framework/log framework {:level level-2 :message "a-2"})
    (framework/log framework {:level level-3 :message "a-3"})
    (let [response (session/message {:op "log-search"
                                     :framework (:id framework)
                                     :appender appender-name
                                     :levels [level-1]})]
      (is (= #{"done"} (:status response)))
      (is (every? #{(name level-1)}
                  (map :level (:search response)))))
    (let [response (session/message {:op "log-search"
                                     :framework (:id framework)
                                     :appender appender-name
                                     :levels [level-1 level-2]})]
      (is (= #{"done"} (:status response)))
      (is (every? #{(name level-1)
                    (name level-2)}
                  (map :level (:search response)))))
    (remove-appender framework appender-name)))

(deftest test-search-by-exception
  (doseq [framework (frameworks)]
    (add-appender framework appender-name)
    (framework/log framework {:message "a-1"})
    (framework/log framework {:message "a-2" :exception (IllegalArgumentException. "BOOM")})
    (framework/log framework {:message "a-3" :exception (IllegalStateException. "BOOM")})
    (let [response (session/message {:op "log-search"
                                     :framework (:id framework)
                                     :appender appender-name
                                     :exceptions ["java.lang.IllegalStateException"]})]
      (let [events (:search response)]
        (is (= 1 (count events)))
        (let [event (first events)]
          (is (uuid-str? (:id event)))
          (is (string? (:level event)))
          (is (string? (:logger event)))
          (is (= "a-3" (:message event)))
          (is (int? (:timestamp event))))))
    (remove-appender framework appender-name)))

(deftest test-search-by-pattern
  (doseq [framework (frameworks)]
    (add-appender framework appender-name)
    (framework/log framework {:message "a-1"})
    (framework/log framework {:message "a-2"})
    (framework/log framework {:message "a-3"})
    (let [response (session/message {:op "log-search"
                                     :framework (:id framework)
                                     :appender appender-name
                                     :pattern "a-3"})]
      (is (= #{"done"} (:status response)))
      (let [events (:search response)]
        (is (= 1 (count events)))
        (let [event (first events)]
          (is (uuid-str? (:id event)))
          (is (= "info" (:level event)))
          (is (string? (:logger event)))
          (is (= "a-3" (:message event)))
          (is (int? (:timestamp event))))))
    (remove-appender framework appender-name)))

(deftest test-search-by-start-and-end-time
  (doseq [framework (frameworks)]
    (add-appender framework appender-name)
    (framework/log framework {:message "a-1"})
    (Thread/sleep 100)
    (framework/log framework {:message "a-2"})
    (Thread/sleep 100)
    (framework/log framework {:message "a-3"})
    (let [response (session/message {:op "log-search"
                                     :framework (:id framework)
                                     :appender appender-name})]
      (is (= #{"done"} (:status response)))
      (let [[event-3 event-2 event-1]
            (:search (session/message {:op "log-search"
                                       :framework (:id framework)
                                       :appender appender-name}))]
        (let [response (session/message {:op "log-search"
                                         :framework (:id framework)
                                         :appender appender-name
                                         :start-time (inc (:timestamp event-1))
                                         :end-time (dec (:timestamp event-3))})]
          (is (= #{"done"} (:status response)))
          (let [events (:search response)]
            (is (= 1 (count events)))
            (let [event (first events)]
              (is (= (:id event-2) (:id event)))
              (is (= "info" (:level event)))
              (is (string? (:logger event)))
              (is (= "a-2" (:message event)))
              (is (int? (:timestamp event))))))))
    (remove-appender framework appender-name)))

(deftest test-threads
  (doseq [framework (frameworks)]
    (add-appender framework appender-name)
    (framework/log framework {:message "a-1"})
    (let [response (session/message {:op "log-threads"
                                     :framework (:id framework)
                                     :appender appender-name})]
      (is (= #{"done"} (:status response)))
      (is (every? keyword? (keys (:threads response))))
      (is (every? pos-int? (vals (:threads response)))))
    (remove-appender framework appender-name)))

(deftest test-remove-appender
  (doseq [framework (frameworks)]
    (testing "remove unregistered appender"
      (let [response (remove-appender framework "unknown")]
        (is (= #{"log-remove-appender-error" "done"} (:status response)))))
    (testing "remove registered appender"
      (add-appender framework appender-name)
      (let [response (remove-appender framework appender-name)]
        (is (= #{"done"} (:status response)))
        (is (= {:consumers []
                :events 0
                :level []
                :name appender-name}
               (:log-remove-appender response)))))))

(deftest test-remove-consumer
  (doseq [framework (frameworks)]
    (add-appender framework appender-name)
    (testing "remove registered consumer"
      (session/message {:op "log-add-consumer"
                        :framework (:id framework)
                        :appender appender-name
                        :consumer "my-consumer"})
      (let [response (session/message {:op "log-remove-consumer"
                                       :framework (:id framework)
                                       :appender appender-name
                                       :consumer "my-consumer"})]
        (is (= #{"done"} (:status response)))
        (is (= {:consumers []
                :events 0
                :level []
                :name appender-name}
               (:remove-consumer response)))))
    (remove-appender framework appender-name)))

(defn log-something [& [n sleep]]
  (doseq [framework (frameworks)
          event (gen/sample (event-gen framework) (or n 1))]
    (framework/log framework event)
    (Thread/sleep (or sleep 10))))

(deftest test-log-something
  (is (nil? (log-something 1))))

(comment (future (log-something 1000 100)))
