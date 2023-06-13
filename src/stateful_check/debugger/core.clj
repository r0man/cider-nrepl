(ns stateful-check.debugger.core
  (:refer-clojure :exclude [print])
  (:require [clojure.spec.alpha :as s]
            [stateful-check.core :as stateful-check]
            [stateful-check.debugger.analyzer :as analyzer]
            [stateful-check.debugger.render :as render]
            [stateful-check.debugger.specs]
            [stateful-check.symbolic-values :as sv])
  (:import [stateful_check.symbolic_values RootVar]))

(defn debugger
  "Return a Stateful Check debugger."
  [& [{:keys [analyzer test]}]]
  {:analyzer analyzer
   :analyses {}
   :last-analysis-id nil
   :test test
   :summary {}})

(defn- failed-event?
  "Return true if `event` is a failed Stateful Check test event,
  otherwise false."
  [event]
  (and (:stateful-check event) (= :fail (:type event))))

(defn ns-specifications
  "Find Stateful Check specifications in `ns` or all namespaces."
  ([]
   (mapcat ns-specifications (map str (all-ns))))
  ([ns]
   (let [ns (symbol ns)]
     (try (require ns) (catch Exception _))
     (->> (ns-publics ns)
          (map (fn [[name var]]
                 {:ns ns
                  :var name
                  :specification (deref var)}))
          (filter #(s/valid? :stateful-check.debugger/specification
                             (:specification %)))))))

(defn test-events
  "Return all Stateful Check test events from the CIDER test `report`."
  [report]
  (->> report :results vals
       (mapcat vals)
       (apply concat)))

(defn- criteria?
  [analysis {:keys [id ns var]}]
  (let [ns (some-> ns symbol)
        var (some-> var symbol)]
    (and (or (nil? id) (= id (:id analysis)))
         (or (nil? ns) (= ns (some-> analysis :test :ns)))
         (or (nil? var) (= var (some-> analysis :test :var))))))

(defn- parse-handle [s]
  (some-> (re-matches #"#<(.+)>" s) second sv/->RootVar))

(defn- to-handle
  "Convert `handle` into a root var."
  [handle]
  (cond
    (instance? RootVar handle)
    handle
    (string? handle)
    (or (parse-handle handle)
        (sv/->RootVar handle))
    :else (throw (ex-info "Invalid command handle" {:handle handle}))))

(defn analyses
  "Return all analyses of the `debugger`."
  [debugger]
  (-> debugger :analyses vals))

(defn get-analysis
  "Find the :analysis of `debugger`."
  [debugger query]
  (cond (uuid? query)
        (get-in debugger [:analyses query])
        (uuid? (:analysis query))
        (get-in debugger [:analyses (:analysis query)])))

(defn get-command
  "Find the command execution for `query` in `debugger`."
  [debugger {:keys [case handle] :as query}]
  (let [prefix (if (= "first" (some-> case name)) [] [:shrunk])]
    (some-> (get-analysis debugger query)
            (get-in (concat prefix [:result-data :environments (to-handle handle)])))))

(defn get-argument
  "Find the command execution argument for `query` in `debugger`."
  [debugger {:keys [argument] :as query}]
  (when (nat-int? argument)
    (some-> (get-command debugger query)
            :arguments (nth argument nil))))

(defn get-result
  "Find the command execution result for `query` in `debugger`."
  [debugger query]
  (when (contains? query :result)
    (when-let [{:keys [result]} (get-command debugger query)]
      (if (:mutated result)
        (:printed-value result)
        (:value result)))))

(defn get-object
  "Find the object for `query` in `debugger`."
  [debugger {:keys [analysis argument handle] :as query}]
  (cond (contains? query :result)
        (get-result debugger query)
        (and analysis argument handle)
        (get-argument debugger query)
        (and analysis handle)
        (get-command debugger query)
        analysis
        (get-analysis debugger query)))

(defn- get-failure-error
  "Find the failure error for `query` in `debugger`."
  [debugger {:keys [failure event] :as query}]
  (when (and (nat-int? failure) (nat-int? event))
    (when-let [command (get-command debugger query)]
      (let [event (get-in (:failures command) [failure :events event] nil)]
        (when (instance? Throwable (:error event))
          (:error event))))))

(defn- get-result-error
  "Find the result error for `query` in `debugger`."
  [debugger query]
  (when-let [{:keys [result]} (get-command debugger query)]
    (when (instance? Throwable (:error result))
      (:error result))))

(defn get-error
  "Find the error for `query` in `debugger`."
  [debugger {:keys [failure event result] :as query}]
  (cond (and (nat-int? failure) (nat-int? event))
        (get-failure-error debugger query)
        result
        (get-result-error debugger query)))

(defn last-analysis
  "Return the last analysis from the `debugger`."
  [debugger]
  (get-analysis debugger (:last-analysis-id debugger)))

(defn- add-analysis [debugger analysis]
  (assoc-in debugger [:analyses (:id analysis)] analysis))

(defn- remove-analysis [debugger analysis]
  (update debugger :analyses dissoc (:id analysis)))

(defn- remove-test-analysis [debugger ns var]
  (let [test-analyses (filter #(criteria? % {:ns ns :var var}) (analyses debugger))]
    (reduce remove-analysis debugger test-analyses)))

(defn filter-analyses
  "Return a new debugger with :analyses filtered by `criteria`"
  [debugger criteria]
  (reduce add-analysis
          (assoc debugger :analyses {})
          (filter #(criteria? % criteria) (analyses debugger))))

(defn analyze-results
  "Analyze the Stateful Check results."
  [debugger results]
  (let [analysis (analyzer/analyze-results results)]
    (-> (add-analysis debugger analysis)
        (assoc :last-analysis-id (:id analysis)))))

(defn analyze-test-event
  "Analyze the Clojure Test `event`."
  [debugger {:keys [ns var] :as event}]
  (let [analysis (analyzer/analyze-test-event event)]
    (-> (remove-test-analysis debugger ns var)
        (add-analysis analysis)
        (assoc :last-analysis-id (:id analysis)))))

(defn analyze-test-report
  "Analyze the Cider test report."
  [debugger report & [opts]]
  (reduce analyze-test-event debugger
          (filter #(criteria? {:test %} opts)
                  (filter failed-event? (test-events report)))))

(defn test-report
  "Analyze the Cider test report."
  [debugger]
  (some->> debugger :test :report deref test-events
           (filter :stateful-check)))

(defn render
  "Render the `debugger` in a Bencode compatible format."
  [debugger]
  (render/render-debugger debugger))

(defn run-specification
  "Run the Stateful Check `specification` and add the analyzed results
  to the `debugger`."
  [debugger specification & [options]]
  (->> (stateful-check/run-specification specification options)
       (analyze-results debugger)))

(defn run-specification-ns+var
  "Run the Stateful Check specification bound to the `var` in `ns`."
  [debugger ns var & [options]]
  (let [var (symbol var)]
    (when-let [{:keys [specification]}
               (some #(and (= var (:var %)) %) (ns-specifications ns))]
      (run-specification debugger specification options))))

(defn print
  "Print the `debugger`.

  Prints the analyzed execution traces of the debugger with
  `stateful-check.report/print-results`."
  [debugger]
  (doseq [{:keys [ns var] :as analysis} (analyses debugger)]
    (printf "Id: %s" (:id analysis))
    (when (and ns var)
      (printf ", Namespace: %s, Var: %s" ns var))
    (println)
    (#'stateful-check/print-execution (:result-data (:shrunk analysis)) false)
    (println)))

(comment

  (require 'cider.nrepl.middleware.test)

  (render my-debugger)

  (def my-debugger
    (analyze-test-report (debugger) @cider.nrepl.middleware.test/current-report))

  (filter-analyses
   my-debugger
   {:ns 'cider.nrepl.middleware.test-stateful-check
    :var 'test-store-record-spec
    })

  )
