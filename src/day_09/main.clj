(ns day-09.main
  (:gen-class)
  (:require [aoc-common-cli :refer [read-intcode-program-from-stdin]]
            [intcode.vm-state :refer [create-intcode-vm]]
            [intcode.run :refer [run]]))

(defn -main [& args]
  (let [program (read-intcode-program-from-stdin)]
    (-> (create-intcode-vm program :inputs [1] :memory-size 1028)
        run
        :outputs
        println)
    (-> (create-intcode-vm program :inputs [2] :memory-size 1076)
        run
        :outputs
        println)))
