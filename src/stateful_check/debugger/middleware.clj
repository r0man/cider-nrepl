(ns stateful-check.debugger.middleware
  (:require [cider.nrepl.middleware.inspect :as middleware.inspect]
            [cider.nrepl.middleware.test :refer [current-report]]
            [cider.nrepl.middleware.util :refer [transform-value]]
            [cider.nrepl.middleware.util.error-handling :refer [with-safe-transport]]
            [haystack.analyzer :as haystack.analyzer]
            [nrepl.middleware.print :as print]
            [nrepl.misc :refer [response-for]]
            [nrepl.transport :as t]
            [orchard.inspect :as inspect]
            [stateful-check.debugger.core :as debugger]
            [stateful-check.debugger.render :as render])
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
                           (inspect/inspect-value value)))}
    :test {:report current-report}}))

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

(defn- stateful-check-analyze-test-reply
  "Handle a Stateful Check test analysis NREPL operation."
  [{:keys [ns var] :as msg}]
  (if (and (string? ns) (string? var))
    (let [ns (symbol ns), var (symbol var)]
      (if-let [report (debugger/find-test-report (debugger msg) ns var)]
        {:stateful-check/analyze-test
         (-> (swap-debugger! msg debugger/analyze-test-event report)
             (debugger/last-analysis)
             (render/render-analysis)
             (transform-value))}
        {:status :stateful-check/test-not-found}))
    {:status :stateful-check/invalid-params}))

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
      {:status :stateful-check/object-not-found
       :query (transform-value query)})))

(defn- stateful-check-run-reply
  "Handle the Stateful Check specification run NREPL operation."
  [{:keys [ns var] :as msg}]
  (if (and (string? ns) (string? var))
    (let [ns (symbol ns)
          var (symbol var)
          options {}]
      {:stateful-check/run
       (-> (swap-debugger! msg debugger/run-specification-var ns var options)
           (debugger/last-analysis)
           (render/render-analysis)
           (transform-value))})
    {:status :stateful-check/specification-not-found}))

(defn- stateful-check-analysis-reply
  "Handle a Stateful Check analysis NREPL operation."
  [{:keys [analysis] :as msg}]
  (let [id (UUID/fromString analysis)]
    (if-let [analysis (debugger/get-analysis (debugger msg) id)]
      {:stateful-check/analysis (render/render-analysis analysis)}
      {:status :stateful-check/analysis-not-found})))

(defn- stateful-check-print-reply
  "Handle a Stateful Check print NREPL operation."
  [{:keys [query ::print/print-fn] :as msg}]
  (if-let [object (debugger/get-object (debugger msg) (parse-query query))]
    (let [writer (StringWriter.)]
      (print-fn object writer)
      {:stateful-check/print (str writer)})
    {:status :stateful-check/object-not-found
     :query (transform-value query)}))

(defn stateful-check-specifications-reply
  "List all Stateful Check specifications from loaded namespaces."
  [_]
  {:stateful-check/specifications
   (map transform-value (debugger/ns-specifications))})

(defn stateful-check-stacktrace-reply
  "Handle a Stateful Check stacktrace NREPL operation."
  [{:keys [query transport ::print/print-fn] :as msg}]
  (if-let [exception (debugger/get-error (debugger msg) (parse-query query))]
    (do (doseq [cause (haystack.analyzer/analyze exception print-fn)]
          (t/send transport (response-for msg cause)))
        (t/send transport (response-for msg :status :done)))
    (t/send transport (response-for msg :status :stateful-check/no-error))))

(defn stateful-check-test-reports-reply
  "List all Stateful Check reports captured by the CIDER test middleware."
  [{:keys [query transport ::print/print-fn] :as msg}]
  {:stateful-check/test-reports
   (->> (debugger/test-report (debugger msg))
        (map #(dissoc % :stateful-check))
        (transform-value))})

(defn handle-message
  "Handle a Stateful Check NREPL `msg`."
  [handler msg]
  (with-safe-transport handler msg
    "stateful-check/analysis" stateful-check-analysis-reply
    "stateful-check/analyze-test" stateful-check-analyze-test-reply
    "stateful-check/inspect" stateful-check-inspect-reply
    "stateful-check/print" stateful-check-print-reply
    "stateful-check/run" stateful-check-run-reply
    "stateful-check/specifications" stateful-check-specifications-reply
    "stateful-check/stacktrace" stateful-check-stacktrace-reply
    "stateful-check/test-reports" stateful-check-test-reports-reply))
