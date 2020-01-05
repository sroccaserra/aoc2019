(ns day-13.main
  (:gen-class)
  (:require [aoc-common-cli :refer [read-intcode-program-from-stdin]]
            [intcode.vm-state :refer [create-intcode-vm]]
            [intcode.run :refer [run]]))

(defn -main [& args]
  (let [program (read-intcode-program-from-stdin)
        vm (create-intcode-vm program :memory-size 2659)]
    (prn (->> (run vm)
             :outputs
             rest rest
             (take-nth 3)
             (filter #(= 2 %))
             count))))
