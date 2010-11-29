(ns alijn.logging
  (:require [clojure.contrib.duck-streams :as duck-streams]))

(defn do-nothing [& more])

(defn file-logger [filename]
  (fn [s]
    (duck-streams/append-spit filename (apply str s "\n"))))

(def *logger* do-nothing)

(defn log [s]
  (*logger* s))

(defn log-fitness [method evaluations fitness]
  (log (format "%s %d %f" method evaluations fitness)))

(defmacro with-logger [logger & body]
  `(binding [*logger* ~logger]
     ~@body))