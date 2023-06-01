(ns stateful-check.debugger.core
  (:require [stateful-check.debugger.analyzer :as analyzer]))

(defn make-debugger
  "Make a Stateful Check Debugger."
  []
  {:summary {} :results {}})

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

(defn- criteria? [event criteria]
  (let [ns (some-> criteria :ns symbol)
        var (some-> criteria :var symbol)]
    (and (or (nil? ns) (= ns (:ns event)))
         (or (nil? var) (= var (:var event))))))

(defn- search-events [criteria events]
  (filter #(criteria? % criteria) events))

(defn- reports
  "Return the Stateful Check reports of the `debugger`."
  [debugger]
  (->> debugger :results vals (mapcat vals)))

(defn- add-report [debugger report]
  (assoc-in debugger [:results (:ns report) (:var report)] report))

(defn filter-reports
  "Return a new debugger with results filtered by `criteria`"
  [debugger criteria]
  (reduce add-report
          (assoc debugger :results {})
          (filter #(criteria? % criteria) (reports debugger))))

(defn analyze-test-report
  "Analyze the Stateful Check events in a Cider test report."
  [debugger test-report & [opts]]
  (reduce (fn [debugger {:keys [ns var] :as event}]
            (let [analysis (analyzer/analyze-test-event event)]
              (assoc-in debugger [:results ns var] analysis)))
          debugger (search-events opts (failed-test-events test-report))))

(defn stateful-check-report
  "Return the Stateful Check reports matching `criteria`."
  [debugger & [criteria]]
  (filter-reports debugger criteria))

(defn inspect-value
  "Return the value in the `debugger` to which `cursor` refers to."
  [debugger cursor]
  (some->> cursor analyzer/parse-cursor (get-in debugger)))

(comment

  (stateful-check-report
   (analyze-test-report (make-debugger) @cider.nrepl.middleware.test/current-report)
   {:ns 'cider.nrepl.middleware.test-stateful-check
    :var 'java-map-passes-sequentially})

  )
