(ns cider.log.specs
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]))

(s/def :cider.log.framework/add-appender-fn ifn?)
(s/def :cider.log.framework/constructor qualified-keyword?)
(s/def :cider.log.framework/id string?)
(s/def :cider.log.framework/javadoc-url string?)
(s/def :cider.log.framework/levels (s/map-of keyword? pos-int?))
(s/def :cider.log.framework/log-fn ifn?)
(s/def :cider.log.framework/name string?)
(s/def :cider.log.framework/remove-appender-fn ifn?)
(s/def :cider.log.framework/website-url string?)

(s/def :cider.log/framework
  (s/keys :req-un [:cider.log.framework/add-appender-fn
                   :cider.log.framework/constructor
                   :cider.log.framework/id
                   :cider.log.framework/javadoc-url
                   :cider.log.framework/levels
                   :cider.log.framework/log-fn
                   :cider.log.framework/name
                   :cider.log.framework/remove-appender-fn
                   :cider.log.framework/website-url]))

(s/def :cider.log.appender/id string?)
(s/def :cider.log.appender/size pos-int?)
(s/def :cider.log.appender/threshold (s/and nat-int? #(< % 100)))

(s/def :cider.log/appender
  (s/keys :req-un [:cider.log.appender/id]
          :opt-un [:cider.log.appender/size
                   :cider.log.appender/threshold]))

(s/def :cider.log/appender-ref
  (s/and #(instance? clojure.lang.Atom %)
         #(s/valid? :cider.log/appender (deref %))))

(s/def :cider.log.consumer/callback ifn?)
(s/def :cider.log.consumer/filter (s/map-of string? any?))
(s/def :cider.log.consumer/id uuid?)

(s/def :cider.log/consumer
  (s/keys :req-un [:cider.log.consumer/id]
          :opt-un [:cider.log.consumer/callback
                   :cider.log.consumer/filter]))

(s/def :cider.log.event/argument any?)
(s/def :cider.log.event/arguments (s/coll-of :cider.log.event/argument :kind vector?))
(s/def :cider.log.event/id uuid?)
(s/def :cider.log.event/level simple-keyword?)
(s/def :cider.log.event/logger string?)
(s/def :cider.log.event/mdc (s/map-of string? string?))
(s/def :cider.log.event/message string?)
(s/def :cider.log.event/thread string?)
(s/def :cider.log.event/timestamp pos-int?)

(s/def :cider.log/event
  (s/keys :req-un [:cider.log.event/arguments
                   :cider.log.event/id
                   :cider.log.event/level
                   :cider.log.event/logger
                   :cider.log.event/mdc
                   :cider.log.event/message
                   :cider.log.event/thread
                   :cider.log.event/timestamp]))

;; cider.log.framework

(s/fdef cider.log.framework/add-appender
  :args (s/cat :framework :cider.log/framework :appender :cider.log/appender)
  :ret :cider.log/framework)

(s/fdef cider.log.framework/remove-appender
  :args (s/cat :framework :cider.log/framework :appender :cider.log/appender)
  :ret :cider.log/framework)

(s/fdef cider.log.framework/update-appender
  :args (s/cat :framework :cider.log/framework :appender :cider.log/appender)
  :ret :cider.log/framework)

(s/fdef cider.log.framework/resolve-framework
  :args (s/cat :framework-sym qualified-symbol?)
  :ret (s/nilable :cider.log/framework))

(s/fdef cider.log.framework/resolve-frameworks
  :args (s/or :arity-0 (s/cat)
              :arity-1 (s/cat :framework-syms (s/coll-of qualified-symbol?)))
  :ret (s/map-of :cider.log.framework/id :cider.log/framework))

;; cider.log.appender

(s/fdef cider.log.appender/make-appender
  :args (s/cat :appender :cider.log/appender)
  :ret :cider.log/framework)

(s/fdef cider.log.appender/add-consumer
  :args (s/cat :appender :cider.log/appender-ref :consumer :cider.log/consumer)
  :ret :cider.log/appender-ref)

(s/fdef cider.log.appender/append
  :args (s/cat :appender :cider.log/appender-ref :event :cider.log/event)
  :ret :cider.log/appender-ref)

(s/fdef cider.log.appender/remove-consumer
  :args (s/cat :appender :cider.log/appender-ref :consumer :cider.log/consumer)
  :ret :cider.log/appender-ref)

(s/fdef cider.log.appender/update-consumer
  :args (s/cat :appender :cider.log/appender-ref :consumer :cider.log/consumer :f ifn?)
  :ret :cider.log/appender-ref)

;; cider.log.appender.base

(s/fdef cider.log.appender.base/make-appender
  :args (s/cat :appender :cider.log/appender)
  :ret :cider.log/framework)

(s/fdef cider.log.appender.base/add-consumer
  :args (s/cat :appender :cider.log/appender :consumer :cider.log/consumer)
  :ret :cider.log/appender)

(s/fdef cider.log.appender.base/append
  :args (s/cat :appender :cider.log/appender :event :cider.log/event)
  :ret :cider.log/appender)

(s/fdef cider.log.appender.base/remove-consumer
  :args (s/cat :appender :cider.log/appender :consumer :cider.log/consumer)
  :ret :cider.log/appender)

(s/fdef cider.log.appender.base/update-consumer
  :args (s/cat :appender :cider.log/appender :consumer :cider.log/consumer :f ifn?)
  :ret :cider.log/appender)

(stest/instrument)
