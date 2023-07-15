(ns stateful-check.debugger.error
  (:require [clojure.spec.alpha :as s]))

(defn invalid-handle
  "Return an exception that the command `handle` is not a valid."
  [handle]
  (ex-info "Invalid command handle" {:handle handle}))

(defn invalid-specification
  "Return an exception indicating that `specification` is not valid."
  [specification]
  (ex-info "Invalid Stateful Check specification"
           (merge (s/explain-data :stateful-check/specification specification)
                  {:type :stateful-check/invalid-specification})))

(defn no-specification-bound
  "Return an exception indicating that `var` is not bound to a Stateful
  Check specification."
  [specification var]
  (ex-info "No Stateful Check specification bound to var"
           (merge (s/explain-data :stateful-check/specification specification)
                  {:type :stateful-check/invalid-specification-var
                   :var var})))

(defn run-not-found
  "Return an exception indicating that the Stateful Check `run` was not
  found."
  [run]
  (ex-info "Stateful Check run not found"
           {:type :stateful-check.debugger/run-not-found
            :run run}))

(defn specification-not-found
  "Return an exception indicating that the Stateful Check specification
  was not found by `id`."
  [id]
  (ex-info "Stateful Check specification not found"
           {:type :stateful-check.debugger/specification-not-found
            :id id}))
