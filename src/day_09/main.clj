(ns day-09.main
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-common-cli :refer [read-lines-from-stdin]]
            [day-09.amp :refer [run-amp-chain run-amp-loop permutations]]))

(defn read-ints-from-line [line]
  (->> (str/split line #",")
       (mapv #(Integer/parseInt %))))

(defn -main [& args]
  (let [program (-> (read-lines-from-stdin)
                    first
                    read-ints-from-line)]
    (println (apply max (for [phase-settings (permutations [0 1 2 3 4])]
                          (run-amp-chain program phase-settings 0)))
             (apply max (for [phase-settings (permutations [5 6 7 8 9])]
                          (run-amp-loop program phase-settings 0))))))
