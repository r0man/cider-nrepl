(ns cider.log.framework
  (:require [cider.log.protocols :as p]))

(def ^:dynamic *frameworks*
  ['cider.log.framework.logback/framework
   ;; 'cider.log.framework.log4j2/framework
   ])

(defn appender [framework name]
  (some #(and (= name (:name %)) %) (p/-appenders framework)))

(defn appenders [framework]
  (p/-appenders framework))

(defn add-appender [framework appender]
  (p/-add-appender framework appender))

(defn log [framework data]
  (p/-log framework data))

(defn remove-appender [framework appender]
  (p/-remove-appender framework appender))

(defn update-appender [framework appender]
  (p/-update-appender framework appender))

(defn update-consumer [framework appender consumer]
  (p/-update-consumer framework appender consumer))

(defn resolve-framework [framework-sym]
  (try ((requiring-resolve (symbol framework-sym)))
       (catch Exception _)))

(defn resolve-frameworks
  ([]
   (resolve-frameworks *frameworks*))
  ([framework-syms]
   (reduce (fn [frameworks constructor]
             (if-let [framework (resolve-framework constructor)]
               (assoc frameworks (:id framework) framework)
               frameworks))
           {} framework-syms)))
