(ns clj-ejml.core
  (:import [org.ejml.ops CommonOps]
           [org.ejml.data DenseMatrix64F]))

;;
;; Matrix creation
;;
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
  ([data num-rows]
     (let [cols (/ (count data) num-rows)]
       (DenseMatrix64F. num-rows cols true (double-array data)))))

(defn row-vector
  "Create a row vector."
  [data]
  (matrix data 1))

(defn col-vector
  "Create a column vector."
  [data]
  (matrix data (count data)))

(defn zeros
  "Create a matrix with all elements set to 0."
  ([size]
     (zeros (first size) (second size)))
  ([rows cols]
     (DenseMatrix64F. rows cols)))

(defn ones
  "Create a matrix with all elements set to 1."
  ([size]
     (ones (first size) (second size)))
  ([rows cols]
     (let [row (take cols (repeat 1))]
       (matrix (take rows (repeat row))))))

(defn eye
  "Return an identity matrix."
  [size]
  (CommonOps/identity size))

(defn diag
  "Create a diagonal matrix."
  [& data]
  (CommonOps/diag (double-array data)))

;;
;; Matrix operations
;;
(defn size
  "Return the number of rows and columns in matrix M."
  [M]
  [(.getNumRows M)
   (.getNumCols M)])

(defn set-value
  "Set the value for the given row and col of the matrix M."
  [M row col value]
  (let [R (.copy M)]
    (.set R row col value)
    R))

(defn transpose
  "Transpose the M matrix."
  [M]
  (let [R (.copy M)]
    (CommonOps/transpose R)
    R))

(defn minus
  "Change the sign of all elements."
  [M]
  (let [R (zeros (size M))]
    (CommonOps/changeSign R)
    R))

(defn inv
  [M]
  (let [R (zeros (size M))]
    (CommonOps/invert M R)
    R))

(defn pinv
  "Computes the pseudo-inverse of the matrix."
  [M]
  (let [R (zeros (size M))]
    (CommonOps/pinv M R)
    R))

(defn add
  "Add matrix A and matrix B."
  [A B]
  (let [R (zeros (size A))]
    (CommonOps/add A B R)
    R))

(defn sub
  "Subtract matrix B from matrix A."
  [A B]
  (let [R (zeros (size A))]
    (CommonOps/sub A B R)
    R))

(defn mul
  "Multiply matrix A by matrix B."
  [A B]
  (let [result-rows (-> A size first)
        result-cols (-> B size second)
        R (zeros result-rows result-cols)]
    (CommonOps/mult A B R)
    R))

(defn emul
  "Multiply element wise matrix A by matrix B."
  [A B]
  (let [R (zeros (size A))]
    (CommonOps/elementMult A B R)
    R))

(defn scale
  "Multiply all elements of matrix M by a scalar n."
  [M n]
  (let [R (zeros (size M))]
    (CommonOps/scale n M R)
    R))

(defn div
  "Divide all elements of matrix A by a scalar n."
  [M n]
  (let [R (zeros (size M))]
    (CommonOps/divide n M R)
    R))

(defn ediv
  "Divide element wise matrix A by matrix B."
  [A B]
  (let [R (zeros (size A))]
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