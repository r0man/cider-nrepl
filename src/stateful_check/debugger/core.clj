(ns stateful-check.debugger.core
  (:refer-clojure :exclude [print])
  (:require [stateful-check.debugger.analyzer :as analyzer]
            [stateful-check.debugger.cursor :as cursor]
            [stateful-check.debugger.render :as render]
            [stateful-check.report :as report]))

(defn make-debugger
  "Make a Stateful Check Debugger."
  [& [{:keys [analyzer]}]]
  {:analyzer (analyzer/analyzer analyzer)
   :summary {}
   :analyses {}})

(defn- failed-event?
  "Return true if `event` is a failed Stateful Check test event,
  otherwise false."
  [event]
  (and (:stateful-check event) (= :fail (:type event))))

(defn- failed-test-events
  "Return all failed Stateful Check test events from the Cider test `report`."
  [report]
  (->> report :results vals
       (mapcat vals)
       (apply concat)
       (filter failed-event?)))

(defn- criteria?
  [analysis {:keys [id ns var]}]
  (let [ns (some-> ns symbol)
        var (some-> var symbol)]
    (and (or (nil? id) (= id (some-> analysis :result-data :id)))
         (or (nil? ns) (= ns (some-> analysis :test :ns)))
         (or (nil? var) (= var (some-> analysis :test :var))))))

(defn analyses
  "Return all analyses of the `debugger`."
  [debugger]
  (-> debugger :analyses vals))

(defn- add-analysis [debugger analysis]
  (assoc-in debugger [:analyses (-> analysis :result-data :id)] analysis))

(defn- remove-analysis [debugger analysis]
  (update debugger :analyses dissoc (-> analysis :result-data :id)))

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
  (let [id (-> results :result-data :id)
        analyzer (analyzer/push-path (:analyzer debugger) :analyses id)
        analysis (analyzer/analyze-results analyzer results)]
    (assoc-in debugger [:analyses id] analysis)))

(defn analyze-test-event
  "Analyze the Clojure Test `event`."
  [debugger {:keys [ns var] :as event}]
  (let [id (-> event :stateful-check :result-data :id)
        analyzer (analyzer/push-path (:analyzer debugger) :analyses id)]
    (-> (remove-test-analysis debugger ns var)
        (add-analysis (analyzer/analyze-test-event analyzer event)))))

(defn analyze-test-report
  "Analyze the Cider test report."
  [debugger report & [opts]]
  (reduce analyze-test-event debugger
          (filter #(criteria? {:test %} opts)
                  (failed-test-events report))))

(defn get-value
  "Return the value in the `debugger` to which `cursor` refers to."
  [debugger cursor]
  (prn (some->> cursor cursor/parse))
  (some->> cursor cursor/parse (get-in debugger)))

(defn render
  "Render the `debugger` in a Bencode compatible format."
  [debugger]
  (render/render-debugger debugger))

(defn print
  "Print the `debugger`.

  Prints the analyzed execution traces of the debugger with
  `stateful-check.report/print-results`."
  [debugger]
  (doseq [{:keys [ns var] :as analysis} (analyses debugger)]
    (printf "Id: %s" (-> analysis :result-data :id))
    (when (and ns var)
      (printf ", Namespace: %s, Var: %s" ns var))
    (println)
    (#'report/print-results nil analysis)
    (println)))

(comment

  (some->> my-cursor cursor/parse)

  (require 'cider.nrepl.middleware.test)

  (render my-debugger)

  (def my-debugger
    (analyze-test-report (make-debugger) @cider.nrepl.middleware.test/current-report))

  (failed-test-events @cider.nrepl.middleware.test/current-report)

  (filter-analyses
   my-debugger
   {:ns 'cider.nrepl.middleware.test-stateful-check
    :var 'java-map-passes-sequentially
    })

  )
