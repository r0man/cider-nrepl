(ns stateful-check.debugger.core
  (:refer-clojure :exclude [print])
  (:require [clojure.spec.alpha :as s]
            [stateful-check.core :as stateful-check]
            [stateful-check.debugger.analyzer :as analyzer]
            [stateful-check.debugger.eval :as eval]
            [stateful-check.debugger.specs]
            [stateful-check.debugger.test-report :as test-report]
            [stateful-check.symbolic-values :as sv])
  (:import [java.util UUID]
           [stateful_check.symbolic_values RootVar]))

(defn debugger
  "Return a Stateful Check debugger."
  [& [{:keys [test]}]]
  {:last-runs []
   :runs {}
   :specifications {}
   :test (cond-> {}
           (instance? clojure.lang.Atom (:report test))
           (assoc :report (:report test)))})

(defn ns-specifications
  "Find Stateful Check specifications in `ns` or all namespaces."
  ([]
   (mapcat ns-specifications (map str (all-ns))))
  ([ns]
   (let [ns (symbol ns)]
     (try (require ns) (catch Exception _))
     (keep (fn [[sym var]]
             (let [spec (try (deref var) (catch Exception _))]
               (when (s/valid? :stateful-check/specification spec)
                 (assoc spec :ns ns :var sym :id (str ns "/" sym) :type :var))))
           (ns-publics ns)))))

(defn- criteria?
  [run {:keys [id ns var]}]
  (let [ns (some-> ns symbol)
        var (some-> var symbol)]
    (and (or (nil? id) (= id (:id run)))
         (or (nil? ns) (= ns (some-> run :test :ns)))
         (or (nil? var) (= var (some-> run :test :var))))))

(defn- parse-handle [s]
  (some-> (re-matches #"#<(.+)>" s) second sv/->RootVar))

(defn- make-handle
  "Convert `handle` into a root var."
  [handle]
  (cond
    (instance? RootVar handle)
    handle
    (string? handle)
    (or (parse-handle handle)
        (sv/->RootVar handle))
    :else (throw (ex-info "Invalid command handle" {:handle handle}))))

(defn runs
  "Return all runs of the `debugger`."
  [debugger]
  (-> debugger :runs vals))

(defn get-run
  "Find the run of `debugger`."
  [debugger query]
  (cond (string? query)
        (get-in debugger [:runs query])
        (string? (:run query))
        (get-in debugger [:runs (:run query)])
        (string? (:id query))
        (get-in debugger [:runs (:id query)])))

(defn get-failing-case
  "Find the failing case for `query` in `debugger`."
  [debugger {:keys [case] :as query}]
  (let [prefix (if (= "first" (some-> case name)) [] [:shrunk])]
    (some-> (get-run debugger query) (get-in (concat prefix [:result-data])))))

(defn get-env
  "Find the command execution for `query` in `debugger`."
  [debugger {:keys [handle] :as query}]
  (when-let [environments (:environments (get-failing-case debugger query))]
    (if handle
      (get environments (make-handle handle))
      environments)))

(defn get-argument
  "Find the argument for `query` in `debugger`."
  [debugger {:keys [argument] :as query}]
  (when (nat-int? argument)
    (some-> (get-env debugger query) :arguments (nth argument nil))))

(defn get-result
  "Find the command execution result for `query` in `debugger`."
  [debugger query]
  (when (contains? query :result)
    (when-let [{:keys [result]} (get-env debugger query)]
      (if (:mutated result)
        (:printed-value result)
        (:value result)))))

(defn get-object
  "Find the object for `query` in `debugger`."
  [debugger {:keys [run argument handle] :as query}]
  (cond (contains? query :result)
        (get-result debugger query)
        (and run argument handle)
        (get-argument debugger query)
        (and run handle)
        (get-env debugger query)
        run
        (get-run debugger query)))

(defn- get-failure-error
  "Find the failure error for `query` in `debugger`."
  [debugger {:keys [failure event] :as query}]
  (when (and (nat-int? failure) (nat-int? event))
    (when-let [command (get-env debugger query)]
      (let [event (get-in (:failures command) [failure :events event] nil)]
        (when (instance? Throwable (:error event))
          (:error event))))))

(defn- get-result-error
  "Find the result error for `query` in `debugger`."
  [debugger query]
  (when-let [{:keys [result]} (get-env debugger query)]
    (when (instance? Throwable (:error result))
      (:error result))))

(defn get-error
  "Find the error for `query` in `debugger`."
  [debugger {:keys [failure event result] :as query}]
  (cond (and (nat-int? failure) (nat-int? event))
        (get-failure-error debugger query)
        result
        (get-result-error debugger query)))

(defn specifications
  "Return the Stateful Check specifications of the `debugger`."
  [debugger]
  (vals (:specifications debugger)))

(defn specification
  "Get the Stateful Check specification of `debugger` by `id`."
  [debugger id]
  (get-in debugger [:specifications id]))

(defn last-run
  "Return the last results from the `debugger`."
  [debugger]
  (get-in debugger [:runs (last (:last-runs debugger))]))

(defn- add-specification
  "Add the Stateful Check `specification` to the debugger."
  [debugger {:keys [id] :as specification}]
  (assoc-in debugger [:specifications id] specification))

(defn- add-run
  "Add the Stateful Check `results` to the debugger."
  [debugger run]
  (assoc-in debugger [:runs (:id run)] run))

(defn- remove-run
  "Remove the Stateful Check `results` from the debugger."
  [debugger results]
  (update debugger :runs dissoc (:id results)))

(defn- remove-test-run [debugger ns var]
  (let [test-analyses (filter #(criteria? % {:ns ns :var var}) (runs debugger))]
    (reduce remove-run debugger test-analyses)))

(defn analyze-run
  "Analyze the Stateful Check results."
  [debugger run]
  (let [results (analyzer/analyze-run run)]
    (-> (add-run debugger results)
        (update :last-runs conj (:id results)))))

(defn analyze-test-run
  "Analyze the Clojure Test `event`."
  [debugger {:keys [ns var] :as event}]
  (let [{:keys [specification] :as results} (analyzer/analyze-test-run event)]
    (-> (remove-test-run debugger ns var)
        (add-specification specification)
        (add-run results)
        (update :last-runs conj (:id results)))))

(defn test-report
  "Return the test report of the `debugger`."
  [debugger]
  (some->> debugger :test :report deref))

(defn find-test-event
  "Find the Stateful Check test report by `id`."
  [debugger id]
  (let [ns (some-> id symbol namespace symbol)
        var (some-> id symbol name symbol)]
    (last (test-report/find-events (test-report debugger) ns var))))

(defn run-specification
  "Run the Stateful Check `specification` and add the analyzed results
  to the `debugger`."
  [debugger id & [options]]
  (if-let [specification (specification debugger id)]
    (->> (assoc (stateful-check/run-specification specification options)
                :specification specification :options options)
         (analyze-run debugger))
    (throw (ex-info "Stateful Check specification not found"
                    {:type :stateful-check/specification-not-found
                     :id id}))))

(defn- find-specification [specifications ns var]
  (some #(and (= ns (:ns %)) (= var (:var %)) %) specifications))

(defn- scan-vars
  "Scan all public vars for Stateful Check specifications."
  [debugger]
  (reduce add-specification debugger (ns-specifications)))

(defn- scan-tests
  "Scan all public vars for Stateful Check specifications."
  [debugger]
  (let [specifications (some-> debugger :test :report deref test-report/specifications)]
    (reduce add-specification debugger specifications)))

(defn scan
  "Scan public vars and test reports for Stateful Check specifications."
  [debugger]
  (-> debugger scan-vars scan-tests))

(defn evaluate-step
  "Evaluate a command execution step."
  [debugger run case]
  (if-let [run (get-run debugger run)]
    (add-run debugger (eval/evaluate run case))
    (throw (ex-info "Stateful Check run not found"
                    {:type :stateful-check/run-not-found
                     :run run}))))

(defn print
  "Print the `debugger`.

  Prints the analyzed execution traces of the debugger with
  `stateful-check.report/print-results`."
  [debugger id & [case]]
  (when-let [results (get-run debugger id)]
    (println "  First failing test case")
    (println "  -----------------------------")
    (#'stateful-check/print-execution (:result-data results) false)
    (println "  Smallest case after shrinking")
    (println "  -----------------------------")
    (#'stateful-check/print-execution (:result-data (:shrunk results)) false)))
