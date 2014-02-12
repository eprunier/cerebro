(ns cerebro.matrix-api-test
  (:use [cerebro.matrix-api])
  (:require [clojure.test :refer [deftest run-tests]]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.compliance-tester :as t]
            [cerebro.core :as c]))

(def test-matrix (m/matrix :cerebro [[1 2] [3 4]]))

(deftest compliance-test
  (binding [clojure.core.matrix/*matrix-implementation* :cerebro]
    (t/instance-test test-matrix)
    #_(t/test-implementation test-matrix)
    #_(t/test-assumptions-for-all-sizes test-matrix)
    #_(t/test-coerce-via-vectors test-matrix)
    #_(t/test-equality test-matrix)
    #_(t/test-methods-existence test-matrix)
    #_(when (t/supports-dimensionality? test-matrix 2)
      (t/matrix-tests-2d test-matrix))
    #_(when (t/supports-dimensionality? test-matrix 1)
      (t/vector-tests-1d test-matrix))
    #_(t/test-array-interop test-matrix)
    #_(t/test-numeric-functions test-matrix)
    #_(t/test-dimensionality test-matrix)
    #_(t/test-row-operations test-matrix)
    #_(test-new-matrices test-matrix)))

#_(deftest compliance-test
  (comp/compliance-test test-matrix))
