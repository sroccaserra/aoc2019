(ns aoc-common-cli
  (:require [clojure.string :as str]))

(defn read-lines-from-stdin []
  (line-seq (java.io.BufferedReader. *in*)))

(defn read-ints-from-line [line]
  (->> (str/split line #",")
       (mapv #(Long/parseLong %))))

(defn read-intcode-program-from-stdin []
  (-> (read-lines-from-stdin)
      first
      read-ints-from-line))
