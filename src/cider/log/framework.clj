(ns cider.log.framework
  (:refer-clojure :exclude [name])
  (:require [cider.log.protocol.framework :as p]))

(def ^:dynamic *frameworks*
  ['cider.log.framework.logback/framework
   'cider.log.framework.java/framework
   ;; 'cider.log.framework.log4j2/framework
   ])

(defn appender [framework name]
  (some #(and (= name (:name %)) %) (p/-appenders framework)))

(defn appenders
  "Return the appenders of the log `framework`."
  [framework]
  (p/-appenders framework))

(defn add-appender
  "Add the log `appender` to the `framework`."
  [framework appender]
  (p/-add-appender framework appender))

(defn levels
  "Return the log levels of the `framework`."
  [framework]
  (p/-levels framework))

(defn log
  "Log the `event` with the `framework`."
  [framework data]
  (p/-log framework data))

(defn description
  "Return the description of the `framework`."
  [framework]
  (p/-description framework))

(defn name
  "Return the name of the `framework`."
  [framework]
  (p/-name framework))

(defn id
  "Return the id of the log `framework`."
  [framework]
  (p/-id framework))

(defn javadoc-url
  "Return the Javadoc url of the log `framework`."
  [framework]
  (p/-javadoc-url framework))

(defn remove-appender
  "Remove the log `appender` from the `framework`."
  [framework appender]
  (p/-remove-appender framework appender))

(defn website-url
  "Return the website url of the log `framework`."
  [framework]
  (p/-website-url framework))

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
