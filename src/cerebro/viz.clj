(ns cerebro.viz
  (:import [org.ejml.ops MatrixVisualization]))

(defn show
  "Opens a windows showing the matrix's state."
  [M & [title]]
  (MatrixVisualization/show M title))
