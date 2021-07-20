(ns day-25.main
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-common-cli :refer [read-intcode-program-from-file]]
            [intcode.vm-state :refer [create-intcode-vm add-inputs drop-outputs]]
            [intcode.run :refer [run-until-needs-input]]))

(defn encode-command [command]
  (concat (map int command) [10]))

(defn decode-output [output]
  (apply str (map char output)))

(defn execute-command [vm command]
  (run-until-needs-input (add-inputs vm (encode-command command))))

(defn repl [vm]
  (println (decode-output (:outputs vm)))
  (let [command (read-line)
        vm' (execute-command (drop-outputs vm) command)]
    (recur vm')))

(defn -main [& args]
  (let [intcode-program (read-intcode-program-from-file "resources/day_25/input.txt")
        vm (create-intcode-vm intcode-program :inputs [] :memory-size 8000)]
    (repl (run-until-needs-input vm))))
