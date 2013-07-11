(ns cerebro.matrix-api-test
  (:use [cerebro.matrix-api])
  (:require [clojure.test :refer [deftest run-tests]]
            [clojure.core.matrix.compliance-tester :as comp]
            [cerebro.core :as c]))

(def test-matrix (c/matrix [1 2 3 4 5 6 7 8 9] 3))

(deftest compliance-test
  (binding [clojure.core.matrix/*matrix-implementation* :cerebro]
    (comp/test-implementation test-matrix)
    (comp/test-array-assumptions test-matrix)
    #_(comp/test-assumptions-for-all-sizes test-matrix)
    (comp/test-coerce-via-vectors test-matrix)
    #_(comp/matrix-tests-2d test-matrix)
    #_(comp/vector-tests-1d test-matrix)
    #_(comp/test-array-interop test-matrix)
    #_(comp/test-numeric-functions test-matrix)
    #_(comp/test-dimensionality test-matrix)
    #_(comp/test-new-matrices test-matrix)))


#_(deftest compliance-test
  (comp/compliance-test (c/matrix [[1 2 3] [4 5 6] [7 8 9]])))
