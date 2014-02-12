(ns cerebro.core
  (:refer-clojure :exclude [get set vector?])
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.implementations :as imp])
  (:import [org.ejml.ops MatrixIO]
           [org.ejml.data DenseMatrix64F]
           [org.ejml.ops CommonOps MatrixIO MatrixVisualization MatrixFeatures]))

;;
;; ## Matrix IO
;;

(defn save-matrix
  "Save a matrix to a binary file."
  [m file]
  (MatrixIO/saveBin m file))

(defn load-matrix
  "Load a matrix from a binary file."
  [file]
  (MatrixIO/loadBin file))

;;
;; ## Matrix creation
;;

(defn matrix
  "This is the core function to create matrices.
   It can be used to create row/column vectors and 2-dimensionnal matrices.

   Exemples :

      (matrix [1 2 3 4 5 6])      => row vector
      (matrix [[1 2 3] [4 5 6]])  => 2x3 matrix
      (matrix [1 2 3 4 5 6] 2)    => 2x3 matrix"
  ([data]
     (if (coll? data)
       (if (number? (first data))
         (DenseMatrix64F. 1 (count data) true (double-array data))
         (->> data
              (map double-array)
              into-array
              DenseMatrix64F.))
       (matrix [data])))
  ([data num-rows]
     (let [cols (/ (count data) num-rows)]
       (DenseMatrix64F. num-rows cols true (double-array data)))))

(defn row-vector
  "Shortcut for creating row vectors."
  [data]
  (matrix data 1))

(defn col-vector
  "Shortcut for creating column vectors."
  [data]
  (matrix data (count data)))

(defn zeros
  "Creates a matrix with all elements set to 0."
  ([size]
     (zeros (first size) (second size)))
  ([num-rows num-cols]
     (DenseMatrix64F. num-rows num-cols)))

(defn ones
  "Creates a matrix with all elements set to 1."
  ([size]
     (ones (first size) (second size)))
  ([rows cols]
     (let [row (take cols (repeat 1))]
       (matrix (take rows (repeat row))))))

(defn eye
  "Returns an identity matrix."
  [size]
  (CommonOps/identity size))

(defn diag
  "Creates a diagonal matrix."
  [& data]
  (CommonOps/diag (double-array data)))

(defn clone
  "Duplicates the matrix."
  [m]
  (.copy m))

(defn submatrix
  [m start-row end-row start-col end-col]
  (CommonOps/extract m start-row end-row start-col end-col))

;;
;; ## Convertion functions
;;

(defn matrix->clj
  "Converts a matrix to a clojure vector."
  [m]
  (vec (.getData m)))

(defn clj->matrix
  "Replace the content of M with the clojure vector v."
  [m v]
  (.setData m (double-array v)))

;;
;; ## Informations on matrix
;;

(defn matrix?
  "Tests if M is a matrix."
  [m]
  (instance? DenseMatrix64F m))

(defn vector?
  "Tests if M is a vector."
  [m]
  (MatrixFeatures/isVector m))

(defn num-rows
  "Returns the number of rows of the matrix."
  [m]
  (.getNumRows m))

(defn num-cols
  "Returns the number of columns of the matrix."
  [m]
  (.getNumCols m))

(defn row-vector?
  "Tests if M is a row vector."
  [m]
  (and (vector? m)
       (= 1 (num-rows m))))

(defn col-vector?
  "Tests if M is a col vector."
  [m]
  (and (vector? m)
       (= 1 (num-cols m))))

(defn size
  "Returns the number of rows and columns in matrix M."
  [m]
  (let [rows (.getNumRows m)
        cols (.getNumCols m)]
    (if (vector? m)
      (if (row-vector? m)
        [cols]
        [rows])
      [rows cols])))

;;
;; ## Content access
;;

(defn row
  "Returns the specified row."
  [m i]
  (CommonOps/extract m i (+ 1 i) 0 (num-cols m)))

(defn rows
  "Returns a vector of single row matrices representing each rows."
  [m]
  (->> m
       matrix->clj
       (partition (num-cols m))
       (map row-vector)))

(defn col
  "Returns the specified column."
  [m i]
  (CommonOps/extract m 0 (num-rows m) i (+ 1 i)))

(defn cols
  "Returns a vector of single row matrices representing each columns."
  [m]
  (let [r (.copy m)]
    (CommonOps/transpose r)
    (rows r)))

(defn set
  "Sets the value for the given row and col and returns the new matrix.
   M is not modified."
  [m row col value]
  (let [r (.copy m)]
    (.set r row col value)
    r))

(defn set!
  "Sets the value for the given row and col in M and returns it.
   M is modified."
  ([m index value]
     (if (col-vector? m)
       (.set m index 0 value)
       (.set m 0 index value)))
  ([m row col value]
     (.set m row col value)))

(defn get
  "Returns an element of the matrix."
  [m row col]
  (.get m row col))

(defn merge-rows
  "Merge row-vectors into a single matrix."
  [& rows]
  (let [r (zeros (count rows) (num-cols (first rows)))]
    (->> rows
         (mapcat matrix->clj)
         (clj->matrix r))
    r))

(defn apply-to-rows
  "Applies the function f to each row of the matrix."
  [m f]
  (->> (map f (rows m))
       (apply merge-rows)))

