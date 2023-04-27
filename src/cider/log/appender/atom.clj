(ns cider.log.appender.atom
  (:refer-clojure :exclude [name])
  (:require [cider.log.appender.base :as base]
            [cider.log.protocol.appender :as p]))

(defrecord AtomAppender [base]
  p/Appender
  (-add-consumer [appender consumer]
    (swap! base p/-add-consumer consumer)
    appender)
  (-append [appender event]
    (swap! base p/-append event)
    appender)
  (-clear [appender]
    (swap! base p/-clear)
    appender)
  (-consumers [_]
    (p/-consumers @base))
  (-event [_ id]
    (p/-event @base id))
  (-events [_]
    (p/-events @base))
  (-filters [_]
    (p/-filters @base))
  (-id [_]
    (p/-id @base))
  (-remove-consumer [appender consumer]
    (swap! base p/-remove-consumer consumer)
    appender)
  (-size [_]
    (p/-size @base))
  (-threshold [_]
    (p/-threshold @base))
  (-update-consumer [appender consumer f]
    (swap! base p/-update-consumer consumer f)
    appender))

(defn make-appender
  "Make an atom appender."
  [appender]
  (AtomAppender. (atom (base/make-appender appender))))
