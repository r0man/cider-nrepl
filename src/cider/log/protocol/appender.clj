(ns cider.log.protocol.appender)

(defprotocol Appender
  (-add-consumer [appender consumer]
    "Add the `consumer` to the log `appender`.")
  (-append [appender event]
    "Append the log `event` to the `appender`.")
  (-clear [appender]
    "Clear the log events of the `appender`.")
  (-consumers [appender]
    "Return the consumer of the log `appender`.")
  (-event [appender id]
    "Lookup the event by `id` from the log `appender`.")
  (-events [appender]
    "Return the log events of the `appender`.")
  (-filters [appender]
    "Return the filters of the `appender`.")
  (-id [appender]
    "Return the id of the log `appender`.")
  (-remove-consumer [appender consumer]
    "Remove the `consumer` from the log `appender`.")
  (-size [appender]
    "Return the size of the `appender`.")
  (-threshold [appender]
    "Return the threshold of the `appender`.")
  (-update-consumer [appender consumer f]
    "Update the `consumer` of the log `appender` with `f`."))
