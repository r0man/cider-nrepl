(ns stateful-check.debugger.middleware
  (:require [cider.nrepl.middleware.inspect :as middleware.inspect]
            [cider.nrepl.middleware.test :refer [current-report]]
            [cider.nrepl.middleware.util :refer [transform-value]]
            [cider.nrepl.middleware.util.error-handling :refer [with-safe-transport]]
            [orchard.inspect :as inspect]
            [stateful-check.debugger.core :as debugger]))

(defn- criteria
  "Make the search criteria map from the NREPL msg."
  [{:keys [ns var]}]
  (cond-> {}
    (or (string? ns) (symbol? ns))
    (assoc :ns (symbol (name ns)))
    (or (string? var) (symbol? var))
    (assoc :ns (symbol (name ns)))))

(defn- debugger
  "Return the Stateful Check debugger from the `msg`."
  [msg]
  (or (-> msg :session meta ::debugger)
      (debugger/make-debugger)))

(defn- swap-debugger!
  "Apply `f` with `args` to the debugger of the NREPL `session`."
  [{:keys [session]} f & args]
  (-> session
      (alter-meta! update ::debugger #(apply f % args))
      (get ::debugger)))

(defn- stateful-check-analyze-reply
  "Handle a Stateful Check test analysis NREPL operation."
  [msg]
  {:stateful-check-analyze
   (transform-value
    (swap-debugger! msg (fn [debugger]
                           (-> (debugger/analyze-test-report
                                debugger @current-report (criteria msg))
                               (debugger/filter-reports criteria)))))})

(defn- stateful-check-inspect-reply
  [{:keys [index] :as msg}]
  (if-let [object (debugger/inspect-value (debugger msg) index)]
    (let [inspector (inspect/start (inspect/fresh) object)]
      (#'middleware.inspect/inspector-response
       msg (middleware.inspect/swap-inspector! msg (constantly inspector))))
    {:status :object-not-found :index index}))

(defn- stateful-check-report-reply
  "Handle a Stateful Check test report NREPL operation."
  [msg]
  (let [debugger (debugger/stateful-check-report (debugger msg) msg)]
    {:stateful-check-report (transform-value debugger)}))

(defn handle-message
  "Handle a Stateful Check NREPL `msg`."
  [handler msg]
  (with-safe-transport handler msg
    "stateful-check-analyze" stateful-check-analyze-reply
    "stateful-check-inspect" stateful-check-inspect-reply
    "stateful-check-report" stateful-check-report-reply))
