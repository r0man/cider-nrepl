(ns stateful-check.debugger.specs
  (:require [clojure.spec.alpha :as s])
  (:import [stateful_check.symbolic_values RootVar LookupVar]))

(s/def :stateful-check.symbolic-values/lookup
  #(instance? LookupVar %))

(s/def :stateful-check.symbolic-values/root
  #(instance? RootVar %))

(s/def :stateful-check/symbolic-value
  (s/or :lookup :stateful-check.symbolic-values/lookup
        :root :stateful-check.symbolic-values/root))

;; Specification

(s/def :stateful-check.specification/command
  (s/or :ifn ifn? :var #(instance? clojure.lang.Var %)))

(s/def :stateful-check.specification/commands
  (s/map-of keyword? :stateful-check.specification/command))

(s/def :stateful-check.specification/setup
  (s/or :ifn ifn? :var #(instance? clojure.lang.Var %)))

(s/def :stateful-check/specification
  (s/keys :req-un [:stateful-check.specification/commands]
          :opt-un [:stateful-check.specification/setup]))

;; Debugger Specification

(s/def :stateful-check.debugger.specification/id string?)
(s/def :stateful-check.debugger.specification/ns simple-symbol?)
(s/def :stateful-check.debugger.specification/type #{:test :var})
(s/def :stateful-check.debugger.specification/var simple-symbol?)

(s/def :stateful-check.debugger/specification
  (s/keys :req-un [:stateful-check.debugger.specification/id
                   :stateful-check.debugger.specification/type
                   :stateful-check.specification/commands]
          :opt-un [:stateful-check.debugger.specification/ns
                   :stateful-check.debugger.specification/var
                   :stateful-check.specification/setup]))

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

(s/def :stateful-check.debugger/last-results
  (s/coll-of string? :kind vector?))

(s/def :stateful-check.debugger/results
  (s/map-of string? :stateful-check.debugger/analysis))

(s/def :stateful-check.debugger/specifications
  (s/map-of string? :stateful-check.debugger/specification))

(s/def :stateful-check/debugger
  (s/keys :req-un [:stateful-check.debugger/last-results
                   :stateful-check.debugger/specifications
                   ;; :stateful-check.debugger/results
                   ]))
