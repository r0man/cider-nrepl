(ns stateful-check.debugger.repl
  (:refer-clojure :exclude [reset!])
  (:require [stateful-check.debugger.core :as debugger]))

(def ^:dynamic *debugger*
  "The debugger used in the REPL."
  (debugger/debugger))

(defn- swap-debugger!
  "Swap the `*debugger*` by applying `f` and `args` to it."
  [f & args]
  (alter-var-root #'*debugger* #(apply f % args)))

(defn last-results
  "Return the last analysis from the `*debugger*`."
  []
  (debugger/last-results *debugger*))

(defn get-results
  "Lookup an analysis in `*debugger*` according to `query`."
  [query]
  (debugger/get-results *debugger* query))

(defn evaluate-step
  "Lookup an analysis in `*debugger*` according to `query`."
  [& {:keys [run case]}]
  (swap-debugger! debugger/evaluate-step
                  (or run (:id (debugger/last-results *debugger*)))
                  case)
  (debugger/last-results *debugger*))

(defn get-command
  "Find the command execution for `query` in `*debugger*`."
  [query]
  (debugger/get-command *debugger* query))

(defn reset!
  "Reset the `*debugger*`."
  []
  (swap-debugger! (constantly (debugger/debugger))))

(defn run-specification
  "Run the `specification` using `options` and return the analyzed results."
  [specification & [options]]
  (swap-debugger! debugger/run-specification specification options)
  (debugger/last-results *debugger*))

(defn analyze-results
  [results]
  (swap-debugger! debugger/analyze-results results)
  (debugger/last-results *debugger*))

(defn scan
  "Scan for Stateful Check specifications."
  []
  (swap-debugger! debugger/scan))
