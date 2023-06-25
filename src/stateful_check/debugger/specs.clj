(ns stateful-check.debugger.specs
  (:require [clojure.spec.alpha :as s]
            [stateful-check.symbolic-values])
  (:import [stateful_check.symbolic_values RootVar LookupVar]))

;; Stateful Check Symbolic Values

(s/def :stateful-check.symbolic-values/lookup
  #(instance? LookupVar %))

(s/def :stateful-check.symbolic-values/root
  #(instance? RootVar %))

(s/def :stateful-check/symbolic-value
  (s/or :lookup :stateful-check.symbolic-values/lookup
        :root :stateful-check.symbolic-values/root))

;; Stateful Check Bindings

(s/def :stateful-check/bindings
  (s/map-of :stateful-check.symbolic-values/root any?))

;; Stateful Check Command

(s/def :stateful-check.command/args
  (s/or :ifn ifn? :var #(instance? clojure.lang.Var %)))

(s/def :stateful-check.command/command
  (s/or :ifn ifn? :var #(instance? clojure.lang.Var %)))

(s/def :stateful-check.command/next-state
  (s/or :ifn ifn? :var #(instance? clojure.lang.Var %)))

(s/def :stateful-check.command/name keyword?)

(s/def :stateful-check/command
  (s/keys :req-un [:stateful-check.command/args
                   :stateful-check.command/command
                   :stateful-check.command/name
                   :stateful-check.command/next-state]))

;; Stateful Check Specification

(s/def :stateful-check.specification/command
  (s/or :map :stateful-check/command :ifn ifn? :var #(instance? clojure.lang.Var %)))

(s/def :stateful-check.specification/commands
  (s/map-of keyword? :stateful-check.specification/command))

(s/def :stateful-check.specification/setup
  (s/or :ifn ifn? :var #(instance? clojure.lang.Var %)))

(s/def :stateful-check/specification
  (s/keys :req-un [:stateful-check.specification/commands]
          :opt-un [:stateful-check.specification/setup]))

;; Stateful Check Run

(s/def :stateful-check.run/depth int?)
(s/def :stateful-check.run/fail (s/coll-of any? :kind vector?))
(s/def :stateful-check.run/failed-after-ms nat-int?)
(s/def :stateful-check.run/failing-size int?)
(s/def :stateful-check.run/num-tests nat-int?)
(s/def :stateful-check.run/pass? boolean?)
(s/def :stateful-check.run/result boolean?)
(s/def :stateful-check.run/result-data any?)
(s/def :stateful-check.run/seed int?)
(s/def :stateful-check.run/smallest (s/coll-of any? :kind vector?))
(s/def :stateful-check.run/time-elapsed-ms nat-int?)
(s/def :stateful-check.run/time-shrinking-ms nat-int?)
(s/def :stateful-check.run/total-nodes-visited nat-int?)

(s/def :stateful-check.run/shrunk
  (s/keys :req-un [:stateful-check.run/depth
                   :stateful-check.run/pass?
                   :stateful-check.run/result
                   :stateful-check.run/result-data
                   :stateful-check.run/smallest
                   :stateful-check.run/time-shrinking-ms
                   :stateful-check.run/total-nodes-visited]))

(s/def :stateful-check/run
  (s/keys :req-un [:stateful-check.run/num-tests
                   :stateful-check.run/pass?
                   :stateful-check.run/seed]
          :opt-un [:stateful-check.run/shrunk
                   :stateful-check.run/fail
                   :stateful-check.run/failed-after-ms
                   :stateful-check.run/failing-size
                   :stateful-check.run/result
                   :stateful-check.run/result-data
                   :stateful-check.run/time-elapsed-ms]))

;; Debugger Specification

(s/def :stateful-check.debugger.specification/id string?)
(s/def :stateful-check.debugger.specification/ns simple-symbol?)
(s/def :stateful-check.debugger.specification/type #{:test :var})
(s/def :stateful-check.debugger.specification/var simple-symbol?)

(s/def :stateful-check.debugger/specification
  (s/merge :stateful-check/specification
           (s/keys :req-un [:stateful-check.debugger.specification/id
                            :stateful-check.debugger.specification/type]
                   :opt-un [:stateful-check.debugger.specification/ns
                            :stateful-check.debugger.specification/var])))

;; Debugger Argument

(s/def :stateful-check.debugger.argument/evaluation any?)
(s/def :stateful-check.debugger.argument/index nat-int?)
(s/def :stateful-check.debugger.argument/name string?)
(s/def :stateful-check.debugger.argument/real any?)
(s/def :stateful-check.debugger.argument/symbolic any?)

(s/def :stateful-check.debugger/argument
  (s/keys :req-un [:stateful-check.debugger.argument/index
                   :stateful-check.debugger.argument/real
                   :stateful-check.debugger.argument/symbolic]
          :opt-un [:stateful-check.debugger.argument/evaluation
                   :stateful-check.debugger.argument/name]))

(s/def :stateful-check.debugger/arguments
  (s/coll-of  :stateful-check.debugger/argument :kind vector?))

;; Debugger Binding

(s/def :stateful-check.debugger.binding/evaluation :stateful-check/bindings)
(s/def :stateful-check.debugger.binding/real :stateful-check/bindings)
(s/def :stateful-check.debugger.binding/symbolic :stateful-check/bindings)

(s/def :stateful-check.debugger/bindings
  (s/keys :req-un [:stateful-check.debugger.binding/real
                   :stateful-check.debugger.binding/symbolic]
          :opt-un [:stateful-check.debugger.binding/evaluation]))

;; Debugger Environment

(s/def :stateful-check.debugger.environment/bindings :stateful-check.debugger/bindings)
(s/def :stateful-check.debugger.environment/command :stateful-check/command)
(s/def :stateful-check.debugger.environment/handle :stateful-check.symbolic-values/root)
(s/def :stateful-check.debugger.environment/index nat-int?)

(s/def :stateful-check.debugger/environment
  (s/keys :req-un [:stateful-check.debugger.environment/bindings
                   :stateful-check.debugger.environment/command
                   :stateful-check.debugger.environment/handle
                   :stateful-check.debugger.environment/index]))

(s/def :stateful-check.debugger/environments
  (s/map-of :stateful-check.symbolic-values/root :stateful-check.debugger/environment))

;; Debugger Result Data

(s/def :stateful-check.debugger.result-data/sequential (s/coll-of any? :kind vector?))
(s/def :stateful-check.debugger.result-data/parallel (s/coll-of :stateful-check.debugger/sequential :kind vector?))

(s/def :stateful-check.debugger/result-data
  (s/keys :req-un []
          :opt-un [:stateful-check.debugger/environments
                   :stateful-check.debugger.result-data/sequential
                   :stateful-check.debugger.result-data/parallel]))

;; Analysis

(s/def :stateful-check.debugger.run/executions (s/map-of keyword? map?))
(s/def :stateful-check.debugger.run/id string?)
(s/def :stateful-check.debugger.run/ns simple-symbol?)
(s/def :stateful-check.debugger.run/options map?)
(s/def :stateful-check.debugger.run/results map?)
(s/def :stateful-check.debugger.run/specification :stateful-check.debugger/specification)
(s/def :stateful-check.debugger.run/test-report map?)
(s/def :stateful-check.debugger.run/var simple-symbol?)

(s/def :stateful-check.debugger/run
  (s/merge :stateful-check/run
           (s/keys :req-un [;; :stateful-check.debugger.run/executions
                            :stateful-check.debugger.run/id
                            ;; :stateful-check.debugger.run/options
                            ;; :stateful-check.debugger.run/results
                            ;; :stateful-check.debugger.run/specification
                            ]
                   :opt-un [:stateful-check.debugger.run/ns
                            :stateful-check.debugger.run/var
                            ;; :stateful-check.debugger.run/test-report
                            ])))

;; Debugger

(s/def :stateful-check.debugger/last-runs
  (s/coll-of string? :kind vector?))

(s/def :stateful-check.debugger/runs
  (s/map-of string? :stateful-check.debugger/run))

(s/def :stateful-check.debugger/specifications
  (s/map-of string? :stateful-check.debugger/specification))

(s/def :stateful-check/debugger
  (s/keys :req-un [:stateful-check.debugger/last-runs
                   :stateful-check.debugger/specifications
                   :stateful-check.debugger/runs]))

;; Frame

(s/def :stateful-check.debugger/frame
  (s/keys :req-un [:stateful-check/bindings]))
