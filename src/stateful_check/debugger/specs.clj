(ns stateful-check.debugger.specs
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))

;; Specification

(s/def :stateful-check.debugger.specification/command
  (s/or :ifn ifn? :var #(instance? clojure.lang.Var %)))

(s/def :stateful-check.debugger.specification/commands
  (s/map-of keyword? :stateful-check.debugger.specification/command))

(s/def :stateful-check.debugger.specification/setup any?)

(s/def :stateful-check.debugger/specification
  (s/keys :req-un [:stateful-check.debugger.specification/commands]
          :opt-un [:stateful-check.debugger.specification/setup]))

;; Analyzer

(s/def :stateful-check.debugger.analyzer/path vector?)
(s/def :stateful-check.debugger.analyzer/render (s/with-gen ifn? #(gen/return str)))

(s/def :stateful-check.debugger/analyzer
  (s/keys :req-un [:stateful-check.debugger.analyzer/path
                   :stateful-check.debugger.analyzer/render]))

;; Analysis

(s/def :stateful-check.debugger.analysis/executions (s/map-of keyword? map?))
(s/def :stateful-check.debugger.analysis/id string?)
(s/def :stateful-check.debugger.analysis/ns simple-symbol?)
(s/def :stateful-check.debugger.analysis/options map?)
(s/def :stateful-check.debugger.analysis/results map?)
(s/def :stateful-check.debugger.analysis/specification :stateful-check.debugger/specification)
(s/def :stateful-check.debugger.analysis/test-report map?)
(s/def :stateful-check.debugger.analysis/var simple-symbol?)

(s/def :stateful-check.debugger/analysis
  (s/keys :req-un [:stateful-check.debugger.analysis/executions
                   :stateful-check.debugger.analysis/id
                   :stateful-check.debugger.analysis/options
                   :stateful-check.debugger.analysis/results
                   :stateful-check.debugger.analysis/specification]
          :opt-un [:stateful-check.debugger.analysis/ns
                   :stateful-check.debugger.analysis/var
                   :stateful-check.debugger.analysis/test-report]))

;; Debugger

(s/def :stateful-check.debugger/results
  (s/map-of string? :stateful-check.debugger/analysis))

(s/def :stateful-check/debugger
  (s/keys :req-un [:stateful-check.debugger/analyzer
                   :stateful-check.debugger/results]))
