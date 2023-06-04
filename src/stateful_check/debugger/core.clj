(ns stateful-check.debugger.core
  (:require [stateful-check.debugger.analyzer :as analyzer]
            [stateful-check.debugger.cursor :as cursor]
            [stateful-check.debugger.render :as render]))

(defn make-debugger
  "Make a Stateful Check Debugger."
  [& [{:keys [analyzer]}]]
  {:analyzer (analyzer/analyzer analyzer)
   :summary {}
   :results {}})

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
  [event {:keys [id ns var] :as criteria}]
  (let [ns (some-> ns symbol)
        var (some-> var symbol)]
    (and (or (nil? id) (= id (:id event)))
         (or (nil? ns) (= ns (:ns event)))
         (or (nil? var) (= var (:var event))))))

(defn- search-events [criteria events]
  (filter #(criteria? % criteria) events))

(defn- reports
  "Return the Stateful Check reports of the `debugger`."
  [debugger]
  (-> debugger :results vals))

(defn- add-report [debugger report]
  (assoc-in debugger [:results (:id report)] report))

(defn filter-results
  "Return a new debugger with results filtered by `criteria`"
  [debugger criteria]
  (reduce add-report
          (assoc debugger :results {})
          (filter #(criteria? % criteria) (reports debugger))))

(defn analyze-test-report
  "Analyze the Stateful Check events in a Cider test report."
  [debugger test-report & [opts]]
  (reduce (fn [{:keys [analyzer] :as debugger} {:keys [ns var] :as event}]
            (let [analyzer (analyzer/push-path analyzer :results (str ns "/" var))
                  analysis (analyzer/analyze-test-report-event analyzer event)]
              (assoc-in debugger [:results (:id analysis)] analysis)))
          debugger (search-events opts (failed-test-events test-report))))

(defn inspect-value
  "Return the value in the `debugger` to which `cursor` refers to."
  [debugger cursor]
  (some->> cursor cursor/parse (get-in debugger)))

(defn render
  "Render the `debugger` in a Bencode compatible format."
  [debugger]
  (render/render-debugger debugger))

(comment

  (some->> my-cursor cursor/parse)

  (def my-debugger
    (analyze-test-report (make-debugger) @cider.nrepl.middleware.test/current-report))

  (failed-test-events @cider.nrepl.middleware.test/current-report)

  (filter-results
   (analyze-test-report (make-debugger) @cider.nrepl.middleware.test/current-report)
   {:ns 'cider.nrepl.middleware.test-stateful-check
    :var 'java-map-passes-sequentially})

  )
