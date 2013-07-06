(ns cerebro.io
  (:require [clojure.data.csv :as csv])
  (:import [org.ejml.ops MatrixIO]))

(defn- as-double
  "Converts a string value to a double"
  [^String value]
  (try
    (Double/valueOf value)
    (catch Exception e
      value)))

(defn- not-empty-line?
  [line]
  (not (and (= (count line) 1)
            (= 0 (-> line first count)))))

(defn load-csv
  "Load CSV file"
  ([filename]
     (load-csv filename []))
  ([filename types]
     (->> filename
          slurp
          csv/read-csv
          (filter not-empty-line?)
          (map #(map as-double %)))))

(defn save-matrix
  "Save a matrix to a binary file."
  [M file]
  (MatrixIO/saveBin M file))

(defn load-matrix
  "Load a matrix from a binary file."
  [file]
  (MatrixIO/loadBin file))
