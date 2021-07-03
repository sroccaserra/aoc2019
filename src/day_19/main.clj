(ns day-19.main
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-common-cli :refer [read-intcode-program-from-file]]
            [intcode.vm-state :refer [create-intcode-vm add-inputs write-int-at]]
            [intcode.run :refer [run run-until-needs-input]]))

(defn -main [& args]
  (let [program (read-intcode-program-from-file "resources/day_19/input.txt")]
    (prn (reduce +
      (for [x (range 0 50)
            y (range 0 50)]
        (let [vm (create-intcode-vm program :inputs [x y] :memory-size 4000)]
          (->> vm run :outputs last)))))))
