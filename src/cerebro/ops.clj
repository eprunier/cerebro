(ns cerebro.ops
  (:refer-clojure :exclude [max min])
  (:require [cerebro.core :as m])
  (:import [org.ejml.ops CommonOps]
           [org.ejml.data DenseMatrix64F]))

;;
;; Matrix operations
;;
(defn transpose
  "Transposes the M matrix."
  [M]
  (let [R (.copy M)]
    (CommonOps/transpose R)
    R))

(defn minus
  "Changes the sign of all elements."
  [M]
  (let [R (.copy M)]
    (CommonOps/changeSign R)
    R))

(defn inv
  "Computes the inverse of the matrix."
  [M]
  (let [R (m/zeros (m/size M))]
    (CommonOps/invert M R)
    R))

(defn pinv
  "Computes the pseudo-inverse of the matrix."
  [M]
  (let [R (m/zeros (m/size M))]
    (CommonOps/pinv M R)
    R))

(defn add
  "Adds matrix A and matrix B. If matrix B is a single row matrix
   then adds B to each row of matrix A."
  [A B]
  (let [R (m/zeros (m/size A))]
    (if (and (> (m/num-rows A) 1)
             (= 1 (m/num-rows B)))
      (m/apply-to-rows A #(add % B))
      (do
        (CommonOps/add A B R)
        R))))

(defn sub
  "Subtracts matrix B from matrix A. If matrix B is a single row
   matrix then subtracts B from each row of matrix A."
  [A B]
  (let [R (m/zeros (m/size A))]
    (if (and (> (m/num-rows A) 1)
             (= 1 (m/num-rows B)))
      (m/apply-to-rows A #(sub % B))
      (do
        (CommonOps/sub A B R)
        R))))

(defn mul
  "Multiplies matrix A by matrix B."
  [A B]
  (if (m/matrix? B)
    (let [R (m/zeros (m/num-rows A) (m/num-cols B))]
      (CommonOps/mult A B R)
      R)
    (let [R (m/zeros (m/size A))]
      (CommonOps/scale B A R)
      R)))

(defn emul
  "Multiplies element wise matrix A by matrix B."
  [A B]
  (let [R (m/zeros (m/size A))]
    (CommonOps/elementMult A B R)
    R))

(defn div
  "Divides all elements of matrix A by a scalar n."
  [M n]
  (let [R (m/zeros (m/size M))]
    (CommonOps/divide n M R)
    R))

(defn ediv
  "Divides element wise matrix A by matrix B."
  [A B]
  (let [R (m/zeros (m/size A))]
    (if (and (> (m/num-rows A) 1)
             (= 1 (m/num-rows B)))
      (m/apply-to-rows A #(ediv % B))
      (do
        (CommonOps/elementDiv A B R)
        R))))

(defn trace
  "Computes the trace of the matrix M."
  [M]
  (CommonOps/trace M))

(defn det
  "Computes the determinant of the matrix M."
  [M]
  (CommonOps/det M))

(defn sum
  "Computes the sums of each columns and returns them in a row vector."
  [M]
  (let [R (m/zeros 1 (m/num-cols M))]
    (CommonOps/sumCols M R)
    R))

(defn mean
  "Computes the mean of each column and return them in a row vector."
  [M]
  (let [R (m/zeros 1 (.getNumCols M))
        nb-rows (.getNumRows M)]
    (CommonOps/sumCols M R)
    (div R nb-rows)))

(defn square
  "Computes the square of each element of the matrix."
  [M]
  (let [T (m/zeros (m/size M))]
    (->> M
         m/matrix->clj
         (map #(* % %))
         (m/clj->matrix T))
    T))

(defn sqrt
  "Computes the square-root of each element of the matrix."
  [M]
  (let [T (m/zeros (m/size M))]
    (->> M
         m/matrix->clj
         (map #(Math/sqrt %))
         (m/clj->matrix T))
    T))

(defn std
  "Computes the standard deviation for each column
   and return them in a row vector.

   std(M) => sqrt(1 / (n - 1) * sum((x - mean)^2))"
  [M]
  (let [R (m/zeros 1 (.getNumCols M))
        nb-rows (.getNumRows M)
        col-means (mean M)]
    (-> (m/apply-to-rows M #(square (sub % col-means)))
        sum
        (mul (/ 1 (- nb-rows 1)))
        sqrt)))

(defn scale
  "Scales matrix data based on standard derivation: (X - means) / std
   and returns a clojure map containing:
      :X = normalized X
      :mu = means
      :sigma = standard derivation
   X is not modified."
  [X]
  (let [mu (mean X)
        sigma (std X)
        X_norm (ediv (sub X mu)
                     sigma)]
    {:X X_norm :mu mu :sigma sigma}))

(defn find-in-cols
  [M f]
  (let [cols (m/cols M)
        values (->> cols
                  (map f)
                  m/row-vector)
        indexes (-> (map #(.indexOf (m/matrix->clj %) %2) 
                         cols (m/matrix->clj values))
                    vec
                    m/row-vector)]
    [values, indexes]))

(defn min
  "Find the minimum value and its index in each columns and returns 
   a Clojure vector containing 2 single row matrices [values indexes]."
  [M]
  (find-in-cols M #(CommonOps/elementMin %)))

(defn max
  "Find the maximum value and its index in each columns and returns 
   a Clojure vector containing 2 single row matrices [values indexes]."
  [M]
  (find-in-cols M #(CommonOps/elementMax %)))
