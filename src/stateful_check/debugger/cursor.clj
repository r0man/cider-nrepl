(ns stateful-check.debugger.cursor
  (:require [clojure.edn :as edn])
  (:import (java.util Base64)))

(defn- base64-encode [s]
  (.encodeToString (Base64/getEncoder) (.getBytes s "UTF-8")))

(defn- base64-decode [s]
  (String. (.decode (Base64/getDecoder) s) "UTF-8"))

(defn cursor
  "Convert `path` into a cursor."
  [path]
  (base64-encode (pr-str path)))

(defn parse
  "Parse the string `s` as a cursor, or throw an exception."
  [s]
  (let [cursor (try (edn/read-string (base64-decode s))
                    (catch Exception _))]
    (if (vector? cursor)
      cursor
      (throw (ex-info (str "Invalid cursor: " s)
                      {:type ::invalid-cursor-error
                       :cursor s})))))
