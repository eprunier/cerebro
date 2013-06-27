(ns cerebro.ops
  (:require [cerebro.core :as core])
  (:import [org.ejml.ops CommonOps]
           [org.ejml.data DenseMatrix64F]))

;;
;; Matrix operations
;;
(defn set-value
  "Set the value for the given row and col of the matrix M."
  [M row col value]
  (let [R (.copy M)]
    (.set R row col value)
    R))

(defn get-value
  [M row col]
  ;; TODO
  )

(defn transpose
  "Transpose the M matrix."
  [M]
  (let [R (.copy M)]
    (CommonOps/transpose R)
    R))

(defn minus
  "Change the sign of all elements."
  [M]
  (let [R (core/zeros (core/size M))]
    (CommonOps/changeSign R)
    R))

(defn inv
  "Computes the inverse of the matrix."
  [M]
  (let [R (core/zeros (core/size M))]
    (CommonOps/invert M R)
    R))

(defn pinv
  "Computes the pseudo-inverse of the matrix."
  [M]
  (let [R (core/zeros (core/size M))]
    (CommonOps/pinv M R)
    R))

(defn add
  "Add matrix A and matrix B."
  [A B]
  (let [R (core/zeros (core/size A))]
    (CommonOps/add A B R)
    R))

(defn sub
  "Subtract matrix B from matrix A."
  [A B]
  (let [R (core/zeros (core/size A))]
    (CommonOps/sub A B R)
    R))

(defn mul
  "Multiply matrix A by matrix B."
  [A B]
  (let [result-rows (-> A core/size first)
        result-cols (-> B core/size second)
        R (core/zeros result-rows result-cols)]
    (CommonOps/mult A B R)
    R))

(defn emul
  "Multiply element wise matrix A by matrix B."
  [A B]
  (let [R (core/zeros (core/size A))]
    (CommonOps/elementMult A B R)
    R))

(defn scale
  "Multiply all elements of matrix M by a scalar n."
  [M n]
  (let [R (core/zeros (core/size M))]
    (CommonOps/scale n M R)
    R))

(defn div
  "Divide all elements of matrix A by a scalar n."
  [M n]
  (let [R (core/zeros (core/size M))]
    (CommonOps/divide n M R)
    R))

(defn ediv
  "Divide element wise matrix A by matrix B."
  [A B]
  (let [R (core/zeros (core/size A))]
    (CommonOps/elementDiv A B R)
    R))

(defn trace
  "Compute the trace of the matrix M."
  [M]
  (CommonOps/trace M))

(defn det
  "Compute the determinant of the matrix M."
  [M]
  (CommonOps/det M))

(defn sum
  [M]
  (CommonOps/elementSum M))

(defn mean
  "Compute the mean of each column and return
   them in a row vector."
  [M]
  (let [R (core/zeros 1 (.getNumCols M))
        nb-rows (.getNumRows M)]
    (CommonOps/sumCols M R)
    (div R nb-rows)))

(defn square
  "Compute the square of each element of the matrix."
  [M]
  (let [T (core/zeros (core/size M))]
    (->> M
         core/matrix->clj
         (map #(* % %))
         (core/clj->matrix T))
    T))

(defn sqrt
  "Compute the square-root of each element of the matrix."
  [M]
  (let [T (core/zeros (core/size M))]
    (->> M
         core/matrix->clj
         (map #(Math/sqrt %))
         (core/clj->matrix T))
    T))

(defn apply-to-rows
  "Apply the function fn to each row of the matrix."
  [M fn]
  (let [R (core/zeros (core/size M))]
    (->> (map fn (core/rows M))
         (mapcat core/matrix->clj)
         vec
         (core/clj->matrix R))
    R))

(defn std
  "Compute the standard deviation for each column
   and return them in a row vector.

   std(M) => sqrt(1 / (n - 1) * sum((x - mean)^2))"
  [M]
  (let [R (core/zeros 1 (.getNumCols M))
        nb-rows (.getNumRows M)
        col-means (mean M)]
    (-> (apply-to-rows M #(square (sub % col-means)))
        (CommonOps/sumCols R))
    (sqrt (scale R (/ 1 (- nb-rows 1))))))
