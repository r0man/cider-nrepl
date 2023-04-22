(ns cider.log.specs
  (:require [clojure.spec.alpha :as s]))

(s/def :cider.log.event/argument any?)
(s/def :cider.log.event/data (s/coll-of :cider.log.event/argument :kind vector?))
(s/def :cider.log.event/id uuid?)
(s/def :cider.log.event/level #{:trace :debug :info :warn :error})
(s/def :cider.log.event/logger string?)
(s/def :cider.log.event/mdc (s/map-of keyword? any?))
(s/def :cider.log.event/message string?)
(s/def :cider.log.event/thread string?)
(s/def :cider.log.event/timestamp pos-int?)

(s/def :cider.log/event
  (s/keys :req-un [:cider.log.event/data
                   :cider.log.event/id
                   :cider.log.event/level
                   :cider.log.event/logger
                   :cider.log.event/mdc
                   :cider.log.event/message
                   :cider.log.event/thread
                   :cider.log.event/timestamp]))
