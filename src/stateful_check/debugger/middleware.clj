(ns stateful-check.debugger.middleware
  (:require [cider.nrepl.middleware.inspect :as middleware.inspect]
            [cider.nrepl.middleware.stacktrace :as middleware.stacktrace]
            [cider.nrepl.middleware.test :refer [current-report]]
            [cider.nrepl.middleware.util :refer [transform-value]]
            [cider.nrepl.middleware.util.error-handling :refer [with-safe-transport]]
            [nrepl.middleware.print :as print]
            [haystack.analyzer :as haystack.analyzer]
            [nrepl.misc :refer [response-for]]
            [nrepl.transport :as t]
            [orchard.inspect :as inspect]
            [stateful-check.debugger.core :as debugger])
  (:import [java.io StringWriter]
           [java.util UUID]))

(defn- criteria
  "Make the search criteria map from the NREPL msg."
  [{:keys [analysis ns var]}]
  (cond-> {}
    (string? analysis)
    (assoc :id (UUID/fromString analysis))
    (or (string? ns) (symbol? ns))
    (assoc :ns (symbol (name ns)))
    (or (string? var) (symbol? var))
    (assoc :var (symbol (name var)))))

(defn- make-debugger
  "Make a new debugger."
  []
  (debugger/debugger
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

(defn render-debugger [debugger]
  (transform-value (debugger/render debugger)))

(defn- stateful-check-analyze-reply
  "Handle a Stateful Check test analysis NREPL operation."
  [msg]
  (let [criteria (criteria msg)]
    {:stateful-check-analyze
     (render-debugger
      (swap-debugger! msg #(-> (debugger/analyze-test-report % @current-report criteria)
                               (debugger/filter-analyses criteria))))}))

(defn- parse-query [query]
  (cond-> query
    (string? (:analysis query))
    (update :analysis #(UUID/fromString %))))

(defn- stateful-check-inspect-reply
  "Handle a Stateful Check inspect NREPL operation."
  [msg]
  (let [query (parse-query (:query msg))]
    (if-let [object (debugger/get-object (debugger msg) query)]
      (let [inspector (inspect/start (inspect/fresh) object)]
        (#'middleware.inspect/inspector-response
         msg (middleware.inspect/swap-inspector! msg (constantly inspector))))
      {:status :stateful-check-object-not-found
       :query (transform-value query)})))

(defn- stateful-check-report-reply
  "Handle a Stateful Check test report NREPL operation."
  [msg]
  (let [debugger (debugger/filter-analyses (debugger msg) msg)]
    {:stateful-check-report (render-debugger debugger)}))

(defn- stateful-check-print-reply
  "Handle a Stateful Check print NREPL operation."
  [{:keys [query ::print/print-fn] :as msg}]
  (if-let [object (debugger/get-object (debugger msg) (parse-query query))]
    (let [writer (StringWriter.)]
      (print-fn object writer)
      {:stateful-check-print (str writer)})
    {:status :stateful-check-object-not-found
     :query (transform-value query)}))

(defn stateful-check-stacktrace-reply
  "Handle a Stateful Check stacktrace NREPL operation."
  [{:keys [query transport ::print/print-fn] :as msg}]
  (if-let [exception (debugger/get-error (debugger msg) (parse-query query))]
    (do (doseq [cause (haystack.analyzer/analyze exception print-fn)]
          (t/send transport (response-for msg cause)))
        (t/send transport (response-for msg :status :done)))
    (t/send transport (response-for msg :status :stateful-check-no-error))))

(defn handle-message
  "Handle a Stateful Check NREPL `msg`."
  [handler msg]
  (with-safe-transport handler msg
    "stateful-check-analyze" stateful-check-analyze-reply
    "stateful-check-inspect" stateful-check-inspect-reply
    "stateful-check-print" stateful-check-print-reply
    "stateful-check-report" stateful-check-report-reply
    "stateful-check-stacktrace" stateful-check-stacktrace-reply))
