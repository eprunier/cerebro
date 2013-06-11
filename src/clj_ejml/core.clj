(ns clj-ejml.core
  (:import [org.ejml.ops CommonOps]
           [org.ejml.data DenseMatrix64F]))

(defn zeros
  [rows cols]
  (DenseMatrix64F. rows cols))

(defn matrix
  "Create a new matrix.

   Create a 2 x 3 matrix :
   (matrix [[1 2 3] [4 5 6]])

   Create the same 2 x 3 matrix :
   (matrix [1 2 3 4 5 6] 2)"
  ([data]
     (->> data
          (map double-array)
          into-array
          DenseMatrix64F.))
  ([data rows]
     (let [cols (/ (count data) rows)]
       (DenseMatrix64F. rows cols true (double-array data)))))

(defn row-vector
  "Create a row vector."
  [data]
  (matrix data 1))

(defn col-vector
  "Create a column vector."
  [data]
  (matrix data (count data)))

(defn set-value
  "Set the value "
  [M row col val]
  (let [T (.copy M)]
    (.set T row col val)
    T))

(defn eye
  "Return an identity matrix."
  [size]
  (CommonOps/identity size))

(defn- with-copy
  [M f]
  (let [T (.copy M)]
    (f T)
    T))


(defn transpose
  "Transpose the M matrix."
  [M]
  (let [T (.copy M)]
    (CommonOps/transpose T)
    T))

(defn negative
  [M]
  (let [T (.copy M)]
    (CommonOps/changeSign T)
    T))

(defn pinv
  [M]
  (let [T (.copy M)]
    (CommonOps/pinv M T)
    T))
