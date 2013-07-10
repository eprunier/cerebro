(ns cerebro.core
  (:refer-clojure :exclude [get set vector?])
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.implementations :as imp])
  (:import [org.ejml.ops MatrixIO]
           [org.ejml.data DenseMatrix64F]
           [org.ejml.ops CommonOps MatrixIO MatrixVisualization MatrixFeatures]))

;;
;; Matrix IO
;;
(defn save-matrix
  "Save a matrix to a binary file."
  [M file]
  (MatrixIO/saveBin M file))

(defn load-matrix
  "Load a matrix from a binary file."
  [file]
  (MatrixIO/loadBin file))

;;
;; Matrix creation
;;
(defn matrix
  "Creates a new matrix.

   Single row matrix exemple: (matrix [1 2 3 4 5 6])
   2 x 3 matrix exemple:      (matrix [[1 2 3] [4 5 6]])
   same 2 x 3 matrix exemple: (matrix [1 2 3 4 5 6] 2)"
  ([data]
     (if (number? (first data))
       (DenseMatrix64F. 1 (count data) true (double-array data))
       (->> data
            (map double-array)
            into-array
            DenseMatrix64F.)))
  ([data num-rows]
     (let [cols (/ (count data) num-rows)]
       (DenseMatrix64F. num-rows cols true (double-array data)))))

(defn row-vector
  "Creates a row vector."
  [data]
  (matrix data 1))

(defn col-vector
  "Creates a column vector."
  [data]
  (matrix data (count data)))

(defn zeros
  "Creates a matrix with all elements set to 0."
  ([size]
     (zeros (first size) (second size)))
  ([rows cols]
     (DenseMatrix64F. rows cols)))

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
  "Duplicates the matrix M."
  [M]
  (.copy M))

(defn extract
  [M start-row end-row start-col end-col]
  (CommonOps/extract M start-row end-row start-col end-col))

;;
;; Convertion functions
;;
(defn matrix->clj
  "Converts a matrix to a clojure vector."
  [M]
  (vec (.getData M)))

(defn clj->matrix
  "Replace the content of M with the clojure vector v."
  [M v]
  (.setData M (double-array v)))

;;
;; Informations on matrix
;;
(defn matrix?
  "Tests if M is a matrix."
  [M]
  (instance? DenseMatrix64F M))

(defn vector?
  "Tests if M is a vector."
  [M]
  (MatrixFeatures/isVector M))

(defn size
  "Returns the number of rows and columns in matrix M."
  [M]
  [(.getNumRows M)
   (.getNumCols M)])

(defn num-rows
  "Returns the number of rows of the matrix."
  [M]
  (.getNumRows M))

(defn num-cols
  "Returns the number of columns of the matrix."
  [M]
  (.getNumCols M))

;;
;; Content access
;;
(defn row
  "Returns the specified row."
  [i M]
  (CommonOps/extract M i (+ 1 i) 0 (num-cols M)))

(defn rows
  "Returns a vector of single row matrices representing each rows."
  [M]
  (->> M
       matrix->clj
       (partition (num-cols M))
       (map row-vector)))

(defn col
  "Returns the specified column."
  [i M]
  (CommonOps/extract M 0 (num-rows M) i (+ 1 i)))

(defn cols
  "Returns a vector of single row matrices representing each columns."
  [M]
  (let [R (.copy M)]
    (CommonOps/transpose R)
    (rows R)))

(defn set
  "Sets the value for the given row and col and returns the new matrix.
   M is not modified."
  [M row col value]
  (let [R (.copy M)]
    (.set R row col value)
    R))

(defn set!
  "Sets the value for the given row and col in M and returns it.
   M is modified."
  [M row col value]
  (.set M row col value))

(defn get
  "Returns an element of the matrix."
  [M row col]
  (.get M row col))

(defn merge-rows
  "Merge row-vectors into a single matrix."
  [& rows]
  (let [R (zeros (count rows) (num-cols (first rows)))]
    (->> rows
         (mapcat matrix->clj)
         (clj->matrix R))
    R))

(defn apply-to-rows
  "Applies the function f to each row of the matrix."
  [M f]
  (->> (map f (rows M))
       (apply merge-rows)))

