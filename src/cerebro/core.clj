(ns cerebro.core
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
;; Convertion functions
;;
(defn matrix->clj
  "Convert a matrix to a clojure vector."
  [M]
  (vec (.getData M)))

(defn clj->matrix
  "Replace the content of M with the clojure vector v."
  [M v]
  (.setData M (double-array v)))

;;
;; Informations on matrix
;;
(defn size
  "Return the number of rows and columns in matrix M."
  [M]
  [(.getNumRows M)
   (.getNumCols M)])

(defn num-rows
  [M]
  (.getNumRows M))

(defn num-cols
  [M]
  (.getNumCols M))

(defn row
  [M i]
  (CommonOps/extract M i (+ 1 i) 0 (num-cols M)))

(defn rows
  [M]
  (let [R (zeros (size M))]
    (->> M
         matrix->clj
         (partition (num-cols M))
         (map row-vector))))
