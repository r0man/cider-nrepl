(ns cider.log.appender-test
  (:require [cider.log.appender :as appender]
            [clojure.test :refer [deftest is]]))

(def event-1
  {:id #uuid "11111111-1111-1111-1111-111111111111"
   :message "Hello world."})

(def event-2
  {:id #uuid "22222222-2222-2222-2222-222222222222"
   :message "Hello world."})

(defn appenders []
  [(appender/make-base-appender {:id "base-appender"})
   (appender/make-atom-appender {:id "atom-appender"})])

(deftest test-append
  (doseq [appender (appenders)]
    (let [appender (appender/append appender event-1)]
      (is (= [event-1] (appender/events appender)))
      (let [appender (appender/append appender event-2)]
        (is (= [event-2 event-1] (appender/events appender)))))))

(deftest test-id
  (doseq [appender (appenders)]
    (is (string? (appender/id appender)))))
