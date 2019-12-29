(ns day-08.main
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-common-cli :refer [read-lines-from-stdin]]))

(defn- read-ints-from-line [line]
  (->> (str/split line #",")
       (mapv #(Integer/parseInt %))))

(defn count-values [coll value]
  (count (filter #(= value %) coll)))

(defn -main [& args]
  (let [layers (->> (read-lines-from-stdin)
                    first
                    (map #(- (int %) (int \0)))
                    (partition (* 25 6))
                    (map (fn [layer] {:layer layer
                                      :nb-zeros (count-values layer 0)
                                      :nb-ones (count-values layer 1)
                                      :nb-twos (count-values layer 2)})))
        layer-with-fewest-zeroes (first (sort-by :nb-zeros layers))]
    (println (* (:nb-ones layer-with-fewest-zeroes)
                (:nb-twos layer-with-fewest-zeroes)))))
