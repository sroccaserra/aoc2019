(ns day-25.main
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-common-cli :refer [read-intcode-program-from-file]]
            [intcode.vm-state :refer [create-intcode-vm add-inputs drop-outputs]]
            [intcode.run :refer [run-until-needs-input]]))

(defn repl [vm]
  (let [command (read-line)
        vm' (run-until-needs-input (add-inputs vm (concat (map int command) [10])))
        result (:outputs vm')
        vm'' (drop-outputs vm')]
    (println (apply str (map char result)))
    (recur vm'')))

(defn -main [& args]
  (let [intcode-program (read-intcode-program-from-file "resources/day_25/input.txt")
        vm (create-intcode-vm intcode-program :inputs [] :memory-size 8000)]
    (repl vm)))
