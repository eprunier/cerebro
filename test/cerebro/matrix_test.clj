(ns cerebro.matrix-test
  (:use [cerebro.matrix])
  (:require [clojure.test :refer [deftest]]
            [clojure.core.matrix.compliance-tester :as comp]
            [cerebro.core :as c]))

(deftest compliance-test
  (comp/compliance-test (c/matrix [[1 2 3] [4 5 6] [7 8 9]])))
