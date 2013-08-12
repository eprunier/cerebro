(ns cerebro.matrix-api
  "Namespace for clojure.core.matrix implementation"
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.implementations :as imp]
            [cerebro.core :as c])
  (:import [org.ejml.data DenseMatrix64F]))

;;
;; ## Implementation of core.matrix protocols
;;

(extend-type org.ejml.data.DenseMatrix64F
  ;;
  ;; Mandatory protocols
  ;;
  mp/PImplementation
  (implementation-key [m]
    :cerebro)
  (construct-matrix [m data]
    (c/matrix data))
  (new-vector [m length]
    (c/zeros length 1))
  (new-matrix [m rows columns]
    (c/zeros rows columns))
  (new-matrix-nd [m shape]
    (c/zeros shape))
  (supports-dimensionality? [m dimensions]
    (<= dimensions 2))

  mp/PDimensionInfo
  (dimensionality [m]
    2)
  (get-shape [m]
    (c/size m))
  (is-scalar? [m]
    false)
  (is-vector? [m]
    (c/vector? m))
  (dimension-count [m dimension-number]
    (condp = dimension-number
      0 (c/num-rows m)
      1 (c/num-cols m)
      (throw (IllegalArgumentException. "Matrix only has dimensions 0 and 1"))))

  mp/PIndexedAccess
  (get-1d [m i]
    (if (c/row-vector? m)
      (c/get m 0 i)
      (c/get m i 0)))
  (get-2d [m row column]
    (c/get m row column))
  (get-nd [m indexes]
    (let [dim (count indexes)]
      (cond (== dim 1) (mp/get-1d m (first indexes))
            (== dim 2) (mp/get-2d m (first indexes) (second indexes))
            :else (throw (UnsupportedOperationException.
                          "Only 2-d get on matrices is supported.")))))

  mp/PIndexedSetting
  (set-1d [m row v]
    (c/set m row 0 v))
  (set-2d [m row column v]
    (c/set m row column v))
  (set-nd [m indexes v]
    (let [dim (count indexes)]
      (cond (== dim 1) (mp/set-1d m (first indexes) v)
            (== dim 2) (mp/set-2d m (first indexes) (second indexes) v)
            :else (throw (UnsupportedOperationException.
                          "Only 1d or 2d set on matrices is supported.")))))
  (is-mutable? [m]
    true)

  mp/PMatrixCloning
  (clone [m]
    (c/clone m))

  ;; 
  ;; =======================================
  ;; Optional protocols
  ;;
  mp/PCoercion
  (coerce-param [m param]
    (if (instance? clojure.lang.PersistentVector param)
        (c/matrix param))))


;;
;; ## Implementation registration
;;

(imp/register-implementation (c/zeros 2 2))