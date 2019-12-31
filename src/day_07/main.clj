(ns day-07.main
  (:gen-class)
  (:require [aoc-common-cli :refer [read-intcode-program-from-stdin]]
            [day-07.amp :refer [run-amp-chain run-amp-loop permutations]]))

(defn -main [& args]
  (let [program (read-intcode-program-from-stdin)]
    (println (apply max (for [phase-settings (permutations [0 1 2 3 4])]
                          (run-amp-chain program phase-settings 0)))
             (apply max (for [phase-settings (permutations [5 6 7 8 9])]
                          (run-amp-loop program phase-settings 0))))))
