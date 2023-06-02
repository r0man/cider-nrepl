(ns stateful-check.debugger.cursor-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [stateful-check.debugger.cursor :as cursor]))

(defspec test-cursor-roundtrip
  (prop/for-all
   [path (gen/vector gen/any-printable-equatable)]
   (= path (cursor/parse (cursor/cursor path)))))
