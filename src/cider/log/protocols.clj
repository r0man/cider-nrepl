(ns cider.log.protocols)

(defprotocol Appender
  (-add-consumer [appender consumer])
  (-append [appender event])
  (-clear [appender])
  (-consumers [appender])
  (-event [appender id])
  (-events [appender])
  (-level [appender])
  (-name [appender])
  (-remove-consumer [appender consumer]))

(defprotocol Framework
  (-add-appender [framework appender])
  (-appenders [framework])
  (-log [framework message])
  (-remove-appender [framework appender])
  (-update-consumer [framework appender consumer]))
