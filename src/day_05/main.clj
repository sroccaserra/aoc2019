(ns day-05.main
  (:gen-class)
  (:require [aoc-common-cli :refer [read-intcode-program-from-stdin]]
            [intcode.vm-state :refer [create-intcode-vm]]
            [intcode.run :refer [run]]))

(defn -main [& args]
  (-> (read-intcode-program-from-stdin)
      (create-intcode-vm :inputs [5])
      run
      prn))
