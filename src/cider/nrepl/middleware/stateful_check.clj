(ns cider.nrepl.middleware.stateful-check
  (:require [stateful-check.debugger.middleware :as middleware]))

(defn handle-stateful-check
  "Handle a Stateful Check NREPL `msg`."
  [handler msg]
  (middleware/handle-message handler msg))
