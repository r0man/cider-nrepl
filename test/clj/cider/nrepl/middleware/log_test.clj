(ns cider.nrepl.middleware.log-test
  (:require [cider.log.framework :as framework]
            [cider.log.specs]
            [cider.nrepl.test-session :as session]
            [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check.generators :as gen]
            [cider.log.event :as event]
            [clojure.pprint :as pp]))

(use-fixtures :each session/session-fixture)

(def appender
  {:id "my-appender"})

(defn frameworks []
  (vals (framework/resolve-frameworks)))

(defn- level-gen [framework]
  (gen/elements (keys (framework/levels framework))))

(defn- event-gen [framework]
  (->> (gen/tuple (s/gen :cider.log/event) (level-gen framework))
       (gen/fmap (fn [[event level]] (assoc event :level level)))))

(defn- uuid-str? [s]
  (try (java.util.UUID/fromString s)
       (catch Exception _)))

(defn- add-appender [framework appender & [opts]]
  (session/message (merge {:op "log-add-appender"
                           :framework (:id framework)
                           :appender (:id appender)}
                          opts)))

(defn- remove-appender [framework appender]
  (session/message {:op "log-remove-appender"
                    :framework (:id framework)
                    :appender (:id appender)}))

(deftest test-add-appender
  (doseq [framework (frameworks)]
    (let [response (add-appender framework appender {:filters {}})]
      (is (= #{"done"} (:status response)))
      (is (= {:consumers []
              :events 0
              :filters {}
              :id (:id appender)}
             (:log-add-appender response))))
    (remove-appender framework appender)))

(deftest test-add-consumer
  (doseq [framework (frameworks)]
    (add-appender framework appender)
    (let [response (session/message {:op "log-add-consumer"
                                     :framework (:id framework)
                                     :appender (:id appender)
                                     :filters {:levels [:info]}})]
      (clojure.pprint/pprint response)
      (is (= #{"done"} (:status response)))
      (let [consumer (:log-add-consumer response)]
        (is (uuid-str? (:id consumer)))
        (is (= {:levels ["info"]} (:filters consumer)))))
    (framework/log framework {:message "a-1"})
    ;; TODO: How to receive the log event?
    (remove-appender framework appender)))

(deftest test-clear
  (doseq [framework (frameworks)]
    (add-appender framework appender)
    (framework/log framework {:message "a-1"})
    (let [response (session/message {:op "log-clear-appender"
                                     :framework (:id framework)
                                     :appender (:id appender)})]
      (is (= #{"done"} (:status response)))
      (is (= {:consumers []
              :events 0
              :filters []
              :id (:id appender)}
             (:log-clear-appender response))))))

(deftest test-exceptions
  (doseq [framework (frameworks)]
    (add-appender framework appender)
    (framework/log framework {:message "a-1" :exception (IllegalArgumentException. "BOOM")})
    (framework/log framework {:message "b-1" :exception (IllegalStateException. "BOOM")})
    (framework/log framework {:message "b-2" :exception (IllegalStateException. "BOOM")})
    (let [response (session/message {:op "log-exceptions"
                                     :framework (:id framework)
                                     :appender (:id appender)})]
      (is (= #{"done"} (:status response)))
      (is (= {:java.lang.IllegalArgumentException 1
              :java.lang.IllegalStateException 2}
             (:log-exceptions response))))
    (remove-appender framework appender)))

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
             (get-in response [:log-frameworks (framework/id framework)]))))))

(deftest test-frameworks-add-appender
  (doseq [framework (frameworks)]
    (add-appender framework appender {:filters {}})
    (let [response (session/message {:op "log-frameworks"})]
      (is (= #{"done"} (:status response)))
      (is (= {:appenders [{:consumers []
                           :events 0
                           :filters {}
                           :id (:id appender)}]
              :description (framework/description framework)
              :id (name (framework/id framework))
              :javadoc-url (framework/javadoc-url framework)
              :name (framework/name framework)
              :website-url (framework/website-url framework)}
             (get-in response [:log-frameworks (framework/id framework)]))))
    (remove-appender framework appender)))

(deftest test-inspect
  (doseq [framework (frameworks)]
    (add-appender framework appender)
    (framework/log framework {:message "a-1"})
    (framework/log framework {:message "a-2"})
    (doseq [event (:log-search
                   (session/message
                    {:op "log-search"
                     :framework (:id framework)
                     :appender (:id appender)}))]
      (let [response (session/message {:op "log-inspect-event"
                                       :framework (:id framework)
                                       :appender (:id appender)
                                       :event-id (:id event)})]
        (is (= #{"done"} (:status response)))
        (is (re-matches (re-pattern (format "(?s).*%s.*" (:id event)))
                        (first (:value response))))))
    (remove-appender framework appender)))

(deftest test-levels
  (doseq [framework (frameworks)
          :let [[level-1 level-2] (reverse (keys (framework/levels framework)))]]
    (add-appender framework appender)
    (framework/log framework {:level level-1 :message "a-1"})
    (framework/log framework {:level level-1 :message "b-1"})
    (framework/log framework {:level level-2 :message "b-2"})
    (let [response (session/message {:op "log-levels"
                                     :framework (:id framework)
                                     :appender (:id appender)})]
      (is (= #{"done"} (:status response)))
      (is (= {level-2 1 level-1 2} (:log-levels response))))
    (remove-appender framework appender)))

(deftest test-loggers
  (doseq [framework (frameworks)]
    (add-appender framework appender)
    (framework/log framework {:logger "LOGGER-A" :message "a-1"})
    (framework/log framework {:logger "LOGGER-B" :message "b-1"})
    (framework/log framework {:logger "LOGGER-B" :message "b-2"})
    (let [response (session/message {:op "log-loggers"
                                     :framework (:id framework)
                                     :appender (:id appender)})]
      (is (= #{"done"} (:status response)))
      (is (= {:LOGGER-A 1 :LOGGER-B 2} (:log-loggers response))))
    (remove-appender framework appender)))

(deftest test-search-by-level
  (doseq [framework (frameworks)
          :let [[level-1 level-2 level-3]
                (reverse (keys (framework/levels framework)))]]
    (add-appender framework appender)
    (framework/log framework {:level level-1 :message "a-1"})
    (framework/log framework {:level level-2 :message "a-2"})
    (framework/log framework {:level level-3 :message "a-3"})
    (let [response (session/message {:op "log-search"
                                     :framework (:id framework)
                                     :appender (:id appender)
                                     :filters {:levels [level-1]}})]
      (is (= #{"done"} (:status response)))
      (is (every? #{(name level-1)}
                  (map :level (:log-search response)))))
    (let [response (session/message {:op "log-search"
                                     :framework (:id framework)
                                     :appender (:id appender)
                                     :filters {:levels [level-1 level-2]}})]
      (is (= #{"done"} (:status response)))
      (let [events (:log-search response)]
        (is (= 2 (count events)))
        (is (every? #{(name level-1)
                      (name level-2)}
                    (map :level events)))))
    (remove-appender framework appender)))

(deftest test-search-by-exception
  (doseq [framework (frameworks)]
    (add-appender framework appender)
    (framework/log framework {:message "a-1"})
    (framework/log framework {:message "a-2" :exception (IllegalArgumentException. "BOOM")})
    (framework/log framework {:message "a-3" :exception (IllegalStateException. "BOOM")})
    (let [response (session/message {:op "log-search"
                                     :framework (:id framework)
                                     :appender (:id appender)
                                     :filters {:exceptions ["java.lang.IllegalStateException"]}})]
      (let [events (:log-search response)]
        (is (= 1 (count events)))
        (let [event (first events)]
          (is (uuid-str? (:id event)))
          (is (string? (:level event)))
          (is (string? (:logger event)))
          (is (= "a-3" (:message event)))
          (is (int? (:timestamp event))))))
    (remove-appender framework appender)))

(deftest test-search-by-pattern
  (doseq [framework (frameworks)]
    (add-appender framework appender)
    (framework/log framework {:message "a-1"})
    (framework/log framework {:message "a-2"})
    (framework/log framework {:message "a-3"})
    (let [response (session/message {:op "log-search"
                                     :framework (:id framework)
                                     :appender (:id appender)
                                     :filters {:pattern "a-3"}})]
      (is (= #{"done"} (:status response)))
      (let [events (:log-search response)]
        (is (= 1 (count events)))
        (let [event (first events)]
          (is (uuid-str? (:id event)))
          (is (= "info" (:level event)))
          (is (string? (:logger event)))
          (is (= "a-3" (:message event)))
          (is (int? (:timestamp event))))))
    (remove-appender framework appender)))

(deftest test-search-by-start-and-end-time
  (doseq [framework (frameworks)]
    (add-appender framework appender)
    (framework/log framework {:message "a-1"})
    (Thread/sleep 100)
    (framework/log framework {:message "a-2"})
    (Thread/sleep 100)
    (framework/log framework {:message "a-3"})
    (let [response (session/message {:op "log-search"
                                     :framework (:id framework)
                                     :appender (:id appender)})]
      (is (= #{"done"} (:status response)))
      (let [[event-3 event-2 event-1]
            (:log-search (session/message {:op "log-search"
                                           :framework (:id framework)
                                           :appender (:id appender)}))]
        (let [response (session/message {:op "log-search"
                                         :framework (:id framework)
                                         :appender (:id appender)
                                         :filters {:start-time (inc (:timestamp event-1))
                                                   :end-time (dec (:timestamp event-3))}})]
          (is (= #{"done"} (:status response)))
          (let [events (:log-search response)]
            (is (= 1 (count events)))
            (let [event (first events)]
              (is (= (:id event-2) (:id event)))
              (is (= "info" (:level event)))
              (is (string? (:logger event)))
              (is (= "a-2" (:message event)))
              (is (int? (:timestamp event))))))))
    (remove-appender framework appender)))

(deftest test-threads
  (doseq [framework (frameworks)]
    (add-appender framework appender)
    (framework/log framework {:message "a-1"})
    (let [response (session/message {:op "log-threads"
                                     :framework (:id framework)
                                     :appender (:id appender)})]
      (is (= #{"done"} (:status response)))
      (let [threads (:log-threads response)]
        (is (every? keyword? (keys threads)))
        (is (every? pos-int? (vals threads)))))
    (remove-appender framework appender)))

(deftest test-remove-appender
  (doseq [framework (frameworks)]
    (testing "remove unregistered appender"
      (let [response (remove-appender framework "unknown")]
        (is (= #{"log-remove-appender-error" "done"} (:status response)))))
    (testing "remove registered appender"
      (add-appender framework appender)
      (let [response (remove-appender framework appender)]
        (is (= #{"done"} (:status response)))
        (is (= {:consumers []
                :events 0
                :filters []
                :id (:id appender)}
               (:log-remove-appender response)))))))

(deftest test-remove-consumer
  (doseq [framework (frameworks)]
    (add-appender framework appender)
    (testing "remove registered consumer"
      (let [consumer (:log-add-consumer
                      (session/message
                       {:op "log-add-consumer"
                        :framework (:id framework)
                        :appender (:id appender)
                        :filters {:levels [:info]}}))
            response (session/message
                      {:op "log-remove-consumer"
                       :framework (:id framework)
                       :appender (:id appender)
                       :consumer (:id consumer)})]
        (is (= #{"done"} (:status response)))
        (is (= {:id (:id consumer)
                :filters (:filters consumer)}
               (:log-remove-consumer response)))
        (let [response (session/message {:op "log-frameworks"})]
          (is (= #{"done"} (:status response)))
          (is (= [{:consumers []
                   :events 0
                   :filters []
                   :id (:id appender)}]
                 (get-in response [:log-frameworks (framework/id framework) :appenders]))))))
    (remove-appender framework appender)))

(deftest test-update-consumer
  (doseq [framework (frameworks)]
    (add-appender framework appender)
    (let [consumer (:log-add-consumer
                    (session/message
                     {:op "log-add-consumer"
                      :framework (:id framework)
                      :appender (:id appender)
                      :filters {:levels [:info]}}))
          response (session/message
                    {:op "log-update-consumer"
                     :framework (:id framework)
                     :appender (:id appender)
                     :consumer (:id consumer)
                     :filters {:levels [:debug]}})]
      (is (= #{"done"} (:status response)))
      (is (= {:id (:id consumer)
              :filters {:levels ["debug"]}}
             (:log-update-consumer response))))
    (remove-appender framework appender)))

(defn log-something [framework & [n sleep]]
  (doseq [event (gen/sample (event-gen framework) (or n 1))]
    (framework/log framework event)
    (Thread/sleep (or sleep 10))) )

(deftest test-log-something
  (doseq [framework (frameworks)]
    (is (nil? (log-something framework 1)))))

(comment (future (log-something (first (frameworks)) 10000 10)))
