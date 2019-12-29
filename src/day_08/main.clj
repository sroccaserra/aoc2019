(ns day-08.main
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-common-cli :refer [read-lines-from-stdin]]))

(defn- read-ints-from-line [line]
  (->> (str/split line #",")
       (mapv #(Integer/parseInt %))))

(def w 25)
(def h 6)
(def transparent-color 2)

(defn- count-values [coll value]
  (count (filter #(= value %) coll)))

(defn- combine-pixels [top-pixel bottom-pixel]
  (if (= transparent-color top-pixel)
    bottom-pixel
    top-pixel))

(defn- combine-layers [top-layer bottom-layer]
  (map combine-pixels top-layer bottom-layer))

(defn -main [& args]
  (let [layers (->> (read-lines-from-stdin)
                    first
                    (map #(- (int %) (int \0)))
                    (partition (* w h))
                    (map vec))
        layer-stats (map (fn [layer] {:nb-zeros (count-values layer 0)
                                      :nb-ones (count-values layer 1)
                                      :nb-twos (count-values layer 2)})
                         layers)
        layer-with-fewest-zeroes (first (sort-by :nb-zeros layer-stats))]
    (println (* (:nb-ones layer-with-fewest-zeroes)
                (:nb-twos layer-with-fewest-zeroes)))
    (doseq [row (partition w (reduce combine-layers layers))]
      (println row))))
