(ns cider.nrepl.middleware.stateful-check-test
  (:require
   [cider.nrepl.middleware.stateful-check :as middleware]
   [clojure.test :refer [deftest is use-fixtures run-tests]]
   [cider.nrepl.middleware.stateful-check-java-map-test :as tests]
   [cider.nrepl.test-session :as session]
   [stateful-check.core :refer [run-specification specification-correct?]]))

(use-fixtures :each session/session-fixture)

;; (defn- make-failed-event [specification]
;;   (let [result (stateful-check.core/run-specification specification)]
;;     {:index 0
;;      :stateful-check.core/spec specification
;;      :stateful-check.core/failed (:fail result)
;;      :stateful-check.core/shrunk (:smallest (:shrunk result))}))

;; (make-failed-event tests/java-map-specification)

(deftest test-stateful-check-render
  (session/message {:op "test"
                    :ns "cider.nrepl.middleware.stateful-check-java-map-test"})
  (clojure.pprint/pprint
   (session/message {:op "stateful-check-render"
                     :ns "cider.nrepl.middleware.stateful-check-java-map-test"
                     :var "java-map-passes-sequentially"}))
  (is true))

;; (deftest test-make-report-sequential
;;   (let [specification tests/java-map-specification
;;         result (run-specification specification)]
;;     (clojure.pprint/pprint (middleware/enhance-report specification result))))

;; (deftest test-make-report-parallel
;;   (let [specification tests/java-map-specification
;;         result (run-specification specification
;;                                   {:gen {:threads 2}
;;                                    :run {:max-tries 100}})]
;;     (def my-result result)
;;     (clojure.pprint/pprint (middleware/enhance-report specification result))))
