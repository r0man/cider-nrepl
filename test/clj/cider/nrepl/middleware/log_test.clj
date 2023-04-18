(ns cider.nrepl.middleware.log-test
  (:require [clojure.test :refer [deftest is join-fixtures use-fixtures]]
            ;; [matcher-combinators.test :refer [match?]]
            [cider.log.framework :as framework]
            ;; [cider.nrepl.test :as test]
            ;; [cider.nrepl.test.session :as session]
            [cider.log.event :as event]
            [clojure.edn :as edn]
            [clojure.string :as str]))

;; (use-fixtures :each (join-fixtures [session/session-fixture test/middleware-fixture]))

;; (defn frameworks []
;;   (vals (framework/resolve-frameworks)))

;; (defn uuid-str? [s]
;;   (try (java.util.UUID/fromString s)
;;        (catch Exception _)))

;; (defn- add-appender [framework appender & [opts]]
;;   (session/message (merge {:op "stem.log/add-appender"
;;                            :framework (:id framework)
;;                            :appender appender}
;;                           opts)))

;; (defn- remove-appender [framework appender]
;;   (session/message {:op "stem.log/remove-appender"
;;                     :framework (:id framework)
;;                     :appender appender}))

;; (deftest test-add-appender
;;   (doseq [framework (frameworks)]
;;     (is (match? {:status #{"done"}
;;                  :stem.log/add-appender {:name "my-appender" :level "debug"}}
;;                 (add-appender framework "my-appender" {:level :debug})))
;;     (is (match? {:status #{"done"}
;;                  :frameworks
;;                  {:logback
;;                   {:appenders [{:name "my-appender" :level "debug"}]
;;                    :description "The Logback logging framework."
;;                    :id "logback"
;;                    :name "Logback"}}}
;;                 (session/message {:op "stem.log/frameworks"})))
;;     (remove-appender framework "my-appender")))

;; (deftest test-add-consumer
;;   (doseq [framework (frameworks)]
;;     (add-appender framework "my-appender")
;;     (is (match? {:status #{"done"}
;;                  :add-consumer {:consumers [{:name "my-consumer"}]
;;                                 :events 0
;;                                 :name "my-appender"}}
;;                 (session/message {:op "stem.log/add-consumer"
;;                                   :framework (:id framework)
;;                                   :appender "my-appender"
;;                                   :consumer "my-consumer"})))
;;     (framework/log framework {:message "a-1"})
;;     ;; TODO: How to receive the log event?
;;     (remove-appender framework "my-appender")))

;; (deftest test-clear
;;   (doseq [framework (frameworks)]
;;     (add-appender framework "my-appender")
;;     (framework/log framework {:message "a-1"})
;;     (is (match? {:status #{"done"}
;;                  :clear-appender {:name "my-appender"
;;                                   :events 0}}
;;                 (session/message {:op "stem.log/clear-appender"
;;                                   :framework (:id framework)
;;                                   :appender "my-appender"})))))

;; (deftest test-exceptions
;;   (doseq [framework (frameworks)]
;;     (add-appender framework "my-appender")
;;     (framework/log framework {:message "a-1" :exception (IllegalArgumentException. "BOOM")})
;;     (framework/log framework {:message "b-1" :exception (IllegalStateException. "BOOM")})
;;     (framework/log framework {:message "b-2" :exception (IllegalStateException. "BOOM")})
;;     (is (match? {:status #{"done"}
;;                  :exceptions {:java.lang.IllegalArgumentException 1
;;                               :java.lang.IllegalStateException 2}}
;;                 (session/message {:op "stem.log/exceptions"
;;                                   :framework (:id framework)
;;                                   :appender "my-appender"})))
;;     (remove-appender framework "my-appender")))

;; (deftest test-frameworks
;;   (doseq [framework (frameworks)]
;;     (is (match? {:status #{"done"}
;;                  :frameworks
;;                  {:logback
;;                   {:description "The Logback logging framework."
;;                    :id "logback"
;;                    :name "Logback"}}}
;;                 (session/message {:op "stem.log/frameworks"})))
;;     (add-appender framework "my-appender")
;;     (remove-appender framework "my-appender")))

;; (deftest test-inspect
;;   (doseq [framework (frameworks)]
;;     (add-appender framework "my-appender")
;;     (framework/log framework {:message "a-1"})
;;     (framework/log framework {:message "a-2"})
;;     (doseq [event (:search (session/message {:op "stem.log/search"
;;                                              :framework (:id framework)
;;                                              :appender "my-appender"}))]
;;       (is (match? {:status #{"done"}
;;                    :value #(re-matches (re-pattern (format "(?s).*%s.*"(:id event)))
;;                                        (str (first %)))}
;;                   (session/message {:op "stem.log/inspect"
;;                                     :framework (:id framework)
;;                                     :appender "my-appender"
;;                                     :event-id (:id event)}))))
;;     (remove-appender framework "my-appender")))

;; (deftest test-levels
;;   (doseq [framework (frameworks)]
;;     (add-appender framework "my-appender")
;;     (framework/log framework {:level :info :message "a-1"})
;;     (framework/log framework {:level :info :message "b-1"})
;;     (framework/log framework {:level :debug :message "b-2"})
;;     (is (match? {:status #{"done"}
;;                  :levels {:debug 1 :info 2}}
;;                 (session/message {:op "stem.log/levels"
;;                                   :framework (:id framework)
;;                                   :appender "my-appender"})))
;;     (remove-appender framework "my-appender")))

;; (deftest test-loggers
;;   (doseq [framework (frameworks)]
;;     (add-appender framework "my-appender")
;;     (session/message {:op "stem.log/attach"})
;;     (framework/log framework {:logger "LOGGER-A" :message "a-1"})
;;     (framework/log framework {:logger "LOGGER-B" :message "b-1"})
;;     (framework/log framework {:logger "LOGGER-B" :message "b-2"})
;;     (is (match? {:status #{"done"}
;;                  :loggers {:LOGGER-A 1 :LOGGER-B 2}}
;;                 (session/message {:op "stem.log/loggers"
;;                                   :framework (:id framework)
;;                                   :appender "my-appender"})))
;;     (remove-appender framework "my-appender")))

;; (deftest test-search-by-level
;;   (doseq [framework (frameworks)]
;;     (add-appender framework "my-appender")
;;     (framework/log framework {:logger "LOGGER-A" :level :info :message "a-1"})
;;     (framework/log framework {:logger "LOGGER-A":level  :warn :message "a-2"})
;;     (framework/log framework {:logger "LOGGER-A" :level :error :message "a-3"
;;                               :exception (IllegalArgumentException. "BOOM")})
;;     (framework/log framework {:logger "LOGGER-B" :level :trace :message "b-1"})
;;     (framework/log framework {:logger "LOGGER-B" :level :debug :message "b-2"})
;;     (framework/log framework {:logger "LOGGER-B" :level :error :message "b-3"
;;                               :exception (IllegalArgumentException. "BOOM")})
;;     (is (match? {:status #{"done"}
;;                  :search [{:id uuid-str?
;;                            :level "debug"
;;                            :logger "LOGGER-B"
;;                            :message "b-2"
;;                            :timestamp int?}
;;                           {:id uuid-str?
;;                            :level "info"
;;                            :logger "LOGGER-A"
;;                            :message "a-1"
;;                            :timestamp int?}]}
;;                 (session/message {:op "stem.log/search"
;;                                   :framework (:id framework)
;;                                   :appender "my-appender"
;;                                   :levels ["INFO" "DEBUG"]})))
;;     (remove-appender framework "my-appender")))

;; (deftest test-search-by-exception
;;   (doseq [framework (frameworks)]
;;     (add-appender framework "my-appender")
;;     (framework/log framework {:logger "LOGGER-A" :level :info :message "a-1"})
;;     (framework/log framework {:logger "LOGGER-A":level  :warn :message "a-2"})
;;     (framework/log framework {:logger "LOGGER-A" :level :error :message "a-3"
;;                               :exception (IllegalArgumentException. "BOOM")})
;;     (framework/log framework {:logger "LOGGER-B" :level :trace :message "b-1"})
;;     (framework/log framework {:logger "LOGGER-B" :level :debug :message "b-2"})
;;     (framework/log framework {:logger "LOGGER-B" :level :error :message "b-3"
;;                               :exception (IllegalStateException. "BOOM")})
;;     (is (match? {:status #{"done"}
;;                  :search [{:id uuid-str?
;;                            :level "error"
;;                            :logger "LOGGER-B"
;;                            :message "b-3"
;;                            :timestamp int?}]}
;;                 (session/message {:op "stem.log/search"
;;                                   :framework (:id framework)
;;                                   :appender "my-appender"
;;                                   :exceptions ["java.lang.IllegalStateException"]})))
;;     (remove-appender framework "my-appender")))

;; (deftest test-search-by-pattern
;;   (doseq [framework (frameworks)]
;;     (add-appender framework "my-appender")
;;     (framework/log framework {:logger "LOGGER-A" :level :info :message "a-1"})
;;     (framework/log framework {:logger "LOGGER-A":level  :warn :message "a-2"})
;;     (framework/log framework {:logger "LOGGER-A" :level :error :message "a-3"
;;                               :exception (IllegalArgumentException. "BOOM")})
;;     (framework/log framework {:logger "LOGGER-B" :level :trace :message "b-1"})
;;     (framework/log framework {:logger "LOGGER-B" :level :debug :message "b-2"})
;;     (framework/log framework {:logger "LOGGER-B" :level :error :message "b-3"
;;                               :exception (IllegalStateException. "BOOM")})
;;     (is (match? {:status #{"done"}
;;                  :search [{:id uuid-str?
;;                            :level "error"
;;                            :logger "LOGGER-B"
;;                            :message "b-3"
;;                            :timestamp int?}]}
;;                 (session/message {:op "stem.log/search"
;;                                   :framework (:id framework)
;;                                   :appender "my-appender"
;;                                   :pattern "b-3"})))
;;     (remove-appender framework "my-appender")))

;; (deftest test-search-by-start-and-end-time
;;   (doseq [framework (frameworks)]
;;     (add-appender framework "my-appender")
;;     (session/message {:op "stem.log/attach"})
;;     (framework/log framework {:message "a-1"})
;;     (Thread/sleep 100)
;;     (framework/log framework {:message "a-2"})
;;     (Thread/sleep 100)
;;     (framework/log framework {:message "a-3"})
;;     (let [[event-3 event-2 event-1]
;;           (:search (session/message {:op "stem.log/search"
;;                                      :framework (:id framework)
;;                                      :appender "my-appender"}))]
;;       (is (match? {:status #{"done"}
;;                    :search [{:id (:id event-2)}]}
;;                   (session/message {:op "stem.log/search"
;;                                     :framework (:id framework)
;;                                     :appender "my-appender"
;;                                     :start-time (inc (:timestamp event-1))
;;                                     :end-time (dec (:timestamp event-3))}))))
;;     (remove-appender framework "my-appender")))

;; (deftest test-threads
;;   (doseq [framework (frameworks)]
;;     (add-appender framework "my-appender")
;;     (framework/log framework {:message "a-1"})
;;     (is (match? {:status #{"done"}
;;                  :threads {(keyword (.getName (Thread/currentThread))) pos-int?}}
;;                 (session/message {:op "stem.log/threads"
;;                                   :framework (:id framework)
;;                                   :appender "my-appender"})))
;;     (remove-appender framework "my-appender")))

;; (deftest test-remove-appender
;;   (doseq [framework (frameworks)]
;;     (add-appender framework "my-appender")
;;     (is (match? {:status #{"done"}
;;                  :stem.log/remove-appender {:name "my-appender"}}
;;                 (remove-appender framework "my-appender")))))

;; (deftest test-remove-consumer
;;   (doseq [framework (frameworks)]
;;     (add-appender framework "my-appender")
;;     (session/message {:op "stem.log/add-consumer"
;;                       :framework (:id framework)
;;                       :appender "my-appender"
;;                       :consumer "my-consumer"})
;;     (is (match? {:status #{"done"}
;;                  :remove-consumer {:consumers []
;;                                    :events 0
;;                                    :name "my-appender"}}
;;                 (session/message {:op "stem.log/remove-consumer"
;;                                   :framework (:id framework)
;;                                   :appender "my-appender"
;;                                   :consumer "my-consumer"})))
;;     (remove-appender framework "my-appender")))
