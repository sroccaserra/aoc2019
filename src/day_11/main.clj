(ns day-11.main
  (:gen-class)
  (:require [aoc-common-cli :refer [read-intcode-program-from-stdin]]
            [intcode.vm-state :refer [create-intcode-vm]]
            [intcode.run :refer [run-until-needs-input]]))

(defn -main [& args]
  (let [program (read-intcode-program-from-stdin)]
    (-> (create-intcode-vm program :inputs [0] :memory-size 1024)
        run-until-needs-input
        :outputs
        println)))
