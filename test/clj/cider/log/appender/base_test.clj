(ns cider.log.appender.base-test
  (:require [cider.log.appender.base :as base]
            [cider.log.specs]
            [clojure.spec.alpha :as s]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]))

(defspec test-clear
  (prop/for-all
   [opts (s/gen :cider.log/appender)
    events (s/gen (s/coll-of :cider.log/event))]
   (-> (reduce base/append (base/make-appender opts) events)
       (base/clear)
       (base/events)
       (empty?))))

(defspec test-event
  (prop/for-all
   [opts (s/gen :cider.log/appender)
    events (s/gen (s/coll-of :cider.log/event))]
   (let [appender (reduce base/append (base/make-appender opts) events)]
     (= (base/events appender)
        (map #(base/event appender (:id %))
             (base/events appender))))))

(defspec test-events
  (prop/for-all
   [opts (s/gen :cider.log/appender)
    events (s/gen (s/coll-of :cider.log/event))]
   (let [appender (reduce base/append (base/make-appender opts) events)]
     (= (take (:size appender) (reverse events))
        (base/events appender)))))
