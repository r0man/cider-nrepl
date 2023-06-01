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

(defn- inspector
  "Return the Stateful Check inspector from the `msg`."
  [msg]
  (or (-> msg :session meta ::inspector)
      (debugger/make-inspector)))

(defn- swap-inspector!
  "Apply `f` with `args` to the inspector of the NREPL `session`."
  [{:keys [session]} f & args]
  (-> session
      (alter-meta! update ::inspector #(apply f % args))
      (get ::inspector)))

(defn- stateful-check-analyze-reply
  "Handle a Stateful Check test analysis NREPL operation."
  [msg]
  {:stateful-check-analyze
   (transform-value
    (swap-inspector! msg (fn [inspector]
                           (-> (debugger/stateful-check-analyze
                                inspector @current-report (criteria msg))
                               (debugger/filter-reports criteria)))))})

(defn- stateful-check-inspect-reply
  [{:keys [index] :as msg}]
  (if-let [object (debugger/get-object (inspector msg) index)]
    (let [inspector (inspect/start (inspect/fresh) object)]
      (#'middleware.inspect/inspector-response
       msg (middleware.inspect/swap-inspector! msg (constantly inspector))))
    {:status :object-not-found :index index}))

(defn- stateful-check-report-reply
  "Handle a Stateful Check test report NREPL operation."
  [msg]
  (let [inspector (debugger/stateful-check-report (inspector msg) msg)]
    {:stateful-check-report (transform-value inspector)}))

(defn handle-message
  "Handle a Stateful Check NREPL `msg`."
  [handler msg]
  (with-safe-transport handler msg
    "stateful-check-analyze" stateful-check-analyze-reply
    "stateful-check-inspect" stateful-check-inspect-reply
    "stateful-check-report" stateful-check-report-reply))
