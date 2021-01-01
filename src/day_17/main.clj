(ns day-17.main
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-common-cli :refer [read-intcode-program-from-file]]
            [intcode.vm-state :refer [create-intcode-vm add-input drop-outputs]]
            [intcode.run :refer [run run-until-needs-input]]))

(defn reset-terminal []
  (println  "\033c"))

(defn display-scaffolding [outputs]
  (->> outputs
      (map char)
      (apply str)
      println))

(defn -main [& args]
  (let [program (read-intcode-program-from-file "resources/day_17/input.txt")
        vm (create-intcode-vm program :inputs [] :memory-size 4000)]
    (display-scaffolding (:outputs (run-until-needs-input vm)))))
