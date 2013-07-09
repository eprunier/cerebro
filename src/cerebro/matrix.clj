(ns cerebro.matrix
  "Namespace for clojure.core.matrix implementation"
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.implementations :as imp]
            [cerebro.core :as c])
  (:import [org.ejml.data DenseMatrix64F]))

;;
;; Types definition
;;
(deftype Matrix [^DenseMatrix64F this]
  )


;;
;; core.matrix implementation
;;
;; =======================================
;; Mandatory protocols
;;
(extend-type Matrix
  mp/PImplementation
  (implementation-key [m]
    :cerebro)
  (construct-matrix [m data]
    (c/matrix data))
  (new-vector [m length]
    )
  (new-matrix [m rows columns]
    )
  (new-matrix-nd [m shape]
    )
  (supports-dimensionality? [m dimensions]
    (<= dimensions 2))

  mp/PDimensionInfo
  (dimensionality [m]
    )
  (get-shape [m]
    )
  (is-scalar? [m]
    false)
  (is-vector? [m]
    )
  (dimension-count [m dimension-number]
    )

  mp/PIndexedAccess
  (get-1d [m i]
    )
  (get-2d [m row column]
    )
  (get-nd [m indexes]
    )

  mp/PIndexedSetting
  (set-1d [m i x]
    )
  (set-2d [m row column x]
    )
  (set-nd [m indexes x]
    )
  (is-mutable? [m]
    )

  mp/PMatrixCloning
  (clone [m]
    (c/clone m)))

;; 
;; =======================================
;; Optional protocols
;;