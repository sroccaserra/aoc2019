(ns day-17.main
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-common-cli :refer [read-intcode-program-from-file]]
            [intcode.vm-state :refer [create-intcode-vm add-input drop-outputs]]
            [intcode.run :refer [run run-until-needs-input]]))

(defn display-scaffolding [lines]
  (->> lines
       (str/join "\n")
       println))

(defn as-lines [outputs]
  (->> outputs
       (map char)
       (apply str)
       str/split-lines))

(defn -main [& args]
  (let [program (read-intcode-program-from-file "resources/day_17/input.txt")
        vm (create-intcode-vm program :memory-size 4000)]
    (-> (:outputs (run-until-needs-input vm))
        as-lines
        display-scaffolding)))
