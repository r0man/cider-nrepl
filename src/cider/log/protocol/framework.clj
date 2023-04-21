(ns cider.log.protocol.framework)

(defprotocol Framework
  (-add-appender [framework appender]
    "Add the log `appender` to the `framework`.")
  (-appenders [framework]
    "Return the appenders of the log `framework`.")
  (-description [framework]
    "Return the description of the log `framework`.")
  (-log [framework event]
    "Log the `event` with the `framework`.")
  (-name [framework]
    "Return the name of the log `framework`.")
  (-id [framework]
    "Return the is of the log `framework`.")
  (-javadoc-url [framework]
    "Return the Javadoc url of the log `framework`.")
  (-remove-appender [framework appender]
    "Remove the log `appender` from the `framework`.")
  (-website-url [framework]
    "Return the website url of the log `framework`."))
