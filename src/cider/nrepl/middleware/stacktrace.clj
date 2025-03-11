(ns cider.nrepl.middleware.stacktrace
  "Cause and stacktrace analysis for exceptions"
  {:author "Jeff Valk"}
  (:require
   [cider.nrepl.middleware.inspect :as middleware.inspect]
   [cider.nrepl.middleware.util.nrepl :refer [notify-client]]
   [nrepl.middleware.print :as print]
   [nrepl.misc :refer [response-for]]
   [nrepl.transport :as t]
   [orchard.stacktrace :as stacktrace]))

(defn- done
  "Send the done response to the client."
  [{:keys [transport] :as msg}]
  (t/send transport (response-for msg :status :done)))

(defn- no-error
  "Send the no error response to the client."
  [{:keys [transport] :as msg}]
  (t/send transport (response-for msg :status :no-error)))

(defn- send-analysis
  "Send the stacktrace analysis response to the client."
  [{:keys [transport] :as msg} analysis]
  (doseq [cause analysis]
    (t/send transport (response-for msg cause))))

;; Analyze the last stacktrace

(def ^:dynamic *last-exception* nil)

(defn- analyze-last-stacktrace
  "Analyze the last exception."
  [{:keys [session ::print/print-fn] :as msg}]
  (let [last-exception (@session #'*e)]
    (swap! session assoc #'*last-exception* last-exception) ;; note that *e can change later, so this entry isn't redundant
    (send-analysis msg (stacktrace/analyze last-exception print-fn))))

(defn- handle-analyze-last-stacktrace-op
  "Handle the analyze last stacktrace op."
  [{:keys [session] :as msg}]
  (if (@session #'*e)
    (analyze-last-stacktrace msg)
    (no-error msg))
  (done msg))

;; Stacktrace

(defn- handle-stacktrace-op
  "Handle the stacktrace op."
  [msg]
  (handle-analyze-last-stacktrace-op msg)
  (notify-client msg "The `stacktrace` op is deprecated, please use `analyze-last-stacktrace` instead." :warning))

(defn handle-inspect-last-exception-op [{:keys [session index transport] :as msg}]
  (let [last-exception (@session #'*last-exception*)
        causes (->> (iterate #(.getCause ^Throwable %) last-exception)
                    (take-while some?))
        cause (when index
                (nth causes index nil))]
    (if cause
      (t/send transport (middleware.inspect/inspect-reply* msg cause))
      (no-error msg))
    (done msg)))

(defn handle-stacktrace
  "Handle stacktrace ops."
  [_ {:keys [op] :as msg}]
  (case op
    "analyze-last-stacktrace" (handle-analyze-last-stacktrace-op msg)
    "inspect-last-exception"  (handle-inspect-last-exception-op msg)
    "stacktrace"              (handle-stacktrace-op msg)))
