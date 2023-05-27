(ns stateful-check.debugger.middleware
  (:require [cider.nrepl.middleware.inspect :as middleware.inspect]
            [cider.nrepl.middleware.test :refer [current-report]]
            [cider.nrepl.middleware.util :refer [transform-value]]
            [cider.nrepl.middleware.util.error-handling :refer [with-safe-transport]]
            [haystack.analyzer :as haystack.analyzer]
            [nrepl.middleware.interruptible-eval :as ie]
            [nrepl.middleware.print :as print]
            [nrepl.misc :refer [response-for]]
            [nrepl.transport :as t]
            [orchard.inspect :as inspect]
            [stateful-check.core :as stateful-check]
            [stateful-check.debugger.core :as debugger]
            [stateful-check.debugger.render :as render]
            [stateful-check.generator :as g])
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
   {:render (fn [value]
              (binding [inspect/*max-atom-length* 50]
                (inspect/inspect-value value)))
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
  [{:keys [test] :as msg}]
  (if-let [event (debugger/find-test-event (debugger msg) test)]
    {:stateful-check/analyze-test
     (-> (swap-debugger! msg debugger/analyze-test-run event)
         (debugger/last-run)
         (render/render-run)
         (transform-value))}
    {:status :stateful-check/test-not-found}))

(defn- parse-query [query]
  (cond-> query
    (string? (:analysis query))
    (update :analysis #(UUID/fromString %))))

(defn- parse-gen-options
  "Parse the Stateful Check generation options."
  [{:keys [threads max-length max-size]}]
  {:max-length (if (nat-int? max-length) max-length g/default-max-length)
   :max-size (if (nat-int? max-size) max-size g/default-max-size)
   :shrink-strategies g/default-shrink-strategies
   :threads (if (nat-int? threads) threads g/default-threads)})

(defn- parse-run-options
  "Parse the Stateful Check run options."
  [{:keys [assume-immutable-results max-tries num-tests seed timeout-ms]}]
  {:assume-immutable-results (= "true" (str assume-immutable-results))
   :max-tries (if (nat-int? max-tries) max-tries stateful-check/default-max-tries)
   :num-tests (if (nat-int? num-tests) num-tests stateful-check/default-num-tests)
   :seed (if (int? seed) seed (System/currentTimeMillis))
   :timeout-ms (if (nat-int? timeout-ms) timeout-ms stateful-check/default-timeout-ms)})

(defn- parse-report-options
  "Parse the Stateful Check report options."
  [{:keys [command-frequency? first-case? stacktrace?]}]
  {:command-frequency? (= "true" (str command-frequency?))
   :first-case? (= "true" (str first-case?))
   :stacktrace? (= "true" (str stacktrace?))})

(defn- parse-options
  "Parse the Stateful Check options."
  [{:keys [gen run report]}]
  {:gen (parse-gen-options gen)
   :run (parse-run-options run)
   :report (parse-report-options report)})

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
  [{:keys [id session specification transport options] :as msg}]
  (let [{:keys [exec]} (meta session)]
    (exec id
          (fn []
            (with-bindings (assoc @session #'ie/*msg* msg)
              (if-let [specification (debugger/specification (debugger msg) specification)]
                (t/send transport (response-for msg :stateful-check/run
                                                (-> (swap-debugger! msg debugger/run-specification (:id specification) (parse-options options))
                                                    (debugger/last-run)
                                                    (render/render-run)
                                                    (transform-value))))
                (t/send transport (response-for msg :status :stateful-check/specification-not-found)))))
          (fn []
            (t/send transport (response-for msg :status :done))))))

(defn stateful-check-scan-reply
  "Scan public vars and test reports for Stateful Check specifications."
  [msg]
  {:stateful-check/scan
   (->> (swap-debugger! msg debugger/scan)
        (debugger/specifications)
        (transform-value))})

(defn- stateful-check-analysis-reply
  "Handle a Stateful Check analysis NREPL operation."
  [{:keys [analysis] :as msg}]
  (let [id (UUID/fromString analysis)]
    (if-let [analysis (debugger/get-run (debugger msg) id)]
      {:stateful-check/analysis (render/render-run analysis)}
      {:status :stateful-check/analysis-not-found})))

(defn- stateful-check-evaluate-step-reply
  "Evaluate a command execution step."
  [{:keys [case run] :as msg}]
  {:stateful-check/evaluate-step
   (-> (swap-debugger! msg debugger/evaluate-step run case)
       (debugger/get-run run)
       (render/render-run)
       (transform-value))})

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
  [msg]
  {:stateful-check/specifications
   (-> (debugger msg)
       (debugger/specifications)
       (transform-value))})

(defn stateful-check-stacktrace-reply
  "Handle a Stateful Check stacktrace NREPL operation."
  [{:keys [query transport ::print/print-fn] :as msg}]
  (if-let [exception (debugger/get-error (debugger msg) (parse-query query))]
    (do (doseq [cause (haystack.analyzer/analyze exception print-fn)]
          (t/send transport (response-for msg cause)))
        (t/send transport (response-for msg :status :done)))
    (t/send transport (response-for msg :status :stateful-check/no-error))))

(defn handle-message
  "Handle a Stateful Check NREPL `msg`."
  [handler msg]
  (case (:op msg)
    "stateful-check/run" (stateful-check-run-reply msg)
    (with-safe-transport handler msg
      "stateful-check/analysis" stateful-check-analysis-reply
      "stateful-check/analyze-test" stateful-check-analyze-test-reply
      "stateful-check/evaluate-step" stateful-check-evaluate-step-reply
      "stateful-check/inspect" stateful-check-inspect-reply
      "stateful-check/print" stateful-check-print-reply
      ;; "stateful-check/run" stateful-check-run-reply
      "stateful-check/scan" stateful-check-scan-reply
      "stateful-check/specifications" stateful-check-specifications-reply
      "stateful-check/stacktrace" stateful-check-stacktrace-reply)))
