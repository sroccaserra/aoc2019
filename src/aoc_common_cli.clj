(ns aoc-common-cli)

(defn read-lines-from-stdin []
  (line-seq (java.io.BufferedReader. *in*)))
