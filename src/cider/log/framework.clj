(ns cider.log.framework
  (:refer-clojure :exclude [name])
  (:require [cider.log.appender :as appender]
            [cider.log.event :as event]))

(def ^:dynamic *frameworks*
  ['cider.log.framework.logback/framework
   'cider.log.framework.jul/framework
   ;; 'cider.log.framework.log4j2/framework
   ])

(defn appenders
  "Return the appenders of the log `framework`."
  [framework]
  (vals (:appenders framework)))

(defn appender [framework id]
  (some #(and (= id (:id (deref %))) %) (appenders framework)))

(defn appender-by-id [framework id]
  (some #(and (= id (:id (deref %))) %) (appenders framework)))

(defn add-appender
  "Add the log `appender` to the `framework`."
  [framework appender]
  (let [appender (appender/make-appender appender)]
    (-> (assoc-in framework [:appenders (:id @appender)] appender)
        ((:add-appender-fn framework) appender))))

(defn levels
  "Return the log levels of the `framework`."
  [framework]
  (:levels framework))

(defn log
  "Log the `event` with the `framework`."
  [framework data]
  ((:log-fn framework) data))

(defn name
  "Return the name of the `framework`."
  [framework]
  (:name framework))

(defn id
  "Return the id of the log `framework`."
  [framework]
  (:id framework))

(defn javadoc-url
  "Return the Javadoc url of the log `framework`."
  [framework]
  (:javadoc-url framework))

(defn remove-appender
  "Remove the log `appender` from the `framework`."
  [framework appender]
  (if-let [appender (get-in framework [:appenders (:id appender)])]
    (-> ((:remove-appender-fn framework) framework appender)
        (update :appenders dissoc (:id @appender)))
    framework))

(defn update-appender
  "Update the `appender` of the log `framework`."
  [framework {:keys [id filters size threshold]}]
  (when-let [appender (get-in framework [:appenders id])]
    (swap! appender assoc
           :filter-fn (event/search-filter filters)
           :filters filters
           :size (or size (appender/size appender))
           :threshold (or threshold (appender/threshold appender)))
    framework))

(defn website-url
  "Return the website url of the log `framework`."
  [framework]
  (:website-url framework))

(defn resolve-framework [framework-sym]
  (try (deref (requiring-resolve (symbol framework-sym)))
       (catch Exception _)))

(defn resolve-frameworks
  ([]
   (resolve-frameworks *frameworks*))
  ([framework-syms]
   (reduce (fn [frameworks constructor]
             (if-let [framework (resolve-framework constructor)]
               (assoc frameworks (id framework) framework)
               frameworks))
           {} framework-syms)))
