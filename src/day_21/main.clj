(ns day-21.main
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-common-cli :refer [read-intcode-program-from-file]]
            [intcode.vm-state :refer [create-intcode-vm add-inputs write-int-at]]
            [intcode.run :refer [run run-until-needs-input]]))

(def program "NOT A J
             NOT C T
             AND D T
             OR T J
             WALK
             ")

(defn -main [& args]
  (let [intcode-program (read-intcode-program-from-file "resources/day_21/input.txt")
        vm (create-intcode-vm intcode-program
                              :inputs (map int program)
                              :memory-size 4000)]
          (->> vm run :outputs (map char) (apply str) print)))
