(ns stateful-check.debugger.middleware
  (:require [cider.nrepl.middleware.inspect :as middleware.inspect]
            [cider.nrepl.middleware.test :refer [current-report]]
            [cider.nrepl.middleware.util :refer [transform-value]]
            [cider.nrepl.middleware.util.error-handling :refer [with-safe-transport]]
            [orchard.inspect :as inspect]
            [stateful-check.debugger.core :as debugger]
            [stateful-check.core :as stateful-check]))

(defn- criteria
  "Make the search criteria map from the NREPL msg."
  [{:keys [ns var]}]
  (cond-> {}
    (or (string? ns) (symbol? ns))
    (assoc :ns (symbol (name ns)))
    (or (string? var) (symbol? var))
    (assoc :var (symbol (name var)))))

(defn- make-debugger
  "Make a new debugger."
  []
  (debugger/make-debugger
   {:analyzer {:render (fn [value]
                         (binding [inspect/*max-atom-length* 50]
                           (inspect/inspect-value value)))}}))

(defn- debugger
  "Return the debugger from `msg` or a new one."
  [msg]
  (or (-> msg :session meta ::debugger) (make-debugger)))

(defn- swap-debugger!
  "Apply `f` with `args` to the debugger of the NREPL `session`."
  [{:keys [session]} f & args]
  (-> session
      (alter-meta! update ::debugger #(apply f (or % (make-debugger)) args))
      (get ::debugger)))

(defn- stateful-check-analyze-reply
  "Handle a Stateful Check test analysis NREPL operation."
  [msg]
  {:stateful-check-analyze
   (transform-value
    (swap-debugger! msg #(-> (debugger/analyze-test-report % @current-report (criteria msg))
                             (debugger/filter-results criteria))))})

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
  (let [debugger (debugger/filter-results (debugger msg) msg)]
    {:stateful-check-report (transform-value debugger)}))

(defn handle-message
  "Handle a Stateful Check NREPL `msg`."
  [handler msg]
  (with-safe-transport handler msg
    "stateful-check-analyze" stateful-check-analyze-reply
    "stateful-check-inspect" stateful-check-inspect-reply
    "stateful-check-report" stateful-check-report-reply))

;; (stateful-check-analyze-reply {:session (atom nil)})
;; (stateful-check-report-reply {})
