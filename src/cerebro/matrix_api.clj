(ns cerebro.matrix-api
  "Namespace for clojure.core.matrix implementation"
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.implementations :as mi]
            [cerebro.core :as c])
  (:import [org.ejml.data DenseMatrix64F]))

;;
;; ## Implementation of core.matrix protocols
;;

(extend-type org.ejml.data.DenseMatrix64F
  ;;
  ;; Mandatory protocols for all implementations
  ;;
  mp/PImplementation
  (implementation-key [m]
    :cerebro)
  (meta-info [m]) ;; TODO
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
    (if (c/vector? m)
      1
      2))
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
      (throw (IllegalArgumentException. "Dimension-number must be 0 or 1."))))

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
                          "Only 1 or 2 dimensions matrices are supported.")))))

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

  ;;
  ;; Mandatory protocols for mutable matrices
  ;;

  mp/PIndexedSettingMutable
  (set-1d! [m index val]) ;; TODO
  (set-2d! [m row col val]) ;; TODO
  (set-nd! [m indices val]) ;; TODO

  mp/PMatrixCloning
  (clone [m]
    (c/clone m))

  ;; 
  ;; Optional protocols
  ;;
  mp/PMatrixSlices
  (get-row [m i]
    (c/row m i))
  (get-column [m i]
    (c/col m i))
  (get-major-slice [m i]
    (mp/get-row m i))
  (get-slice [m dimension i]
    (condp = dimension
      0 (mp/get-row m i)
      1 (mp/get-column m i)
      (throw (UnsupportedOperationException.
              "Dimension must be 0 or 1.")))))


;;
;; ## Implementation registration
;;

(mi/register-implementation (c/zeros 2 2))
