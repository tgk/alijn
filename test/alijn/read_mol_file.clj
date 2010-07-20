(ns alijn.read-mol-file
  (:use [alijn io]))

(defn read-and-print-names [& filenames]
  (let [molecules (read-molecules-from-files filenames)]
    (doseq [molecule molecules]
      (print molecule))))
