(ns day-25.main
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-common-cli :refer [read-intcode-program-from-file]]
            [intcode.vm-state :refer [create-intcode-vm add-inputs drop-outputs]]
            [intcode.run :refer [run-until-needs-input]]))

(defn encode-command [command]
  (concat (map int command) [10]))

(defn execute-command [vm command]
  (run-until-needs-input (add-inputs vm (encode-command command))))

(defn decode-output [output]
  (apply str (map char output)))

(defn print-output [vm]
  (println (decode-output (:outputs vm))))

(defn repl [vm]
  (let [command (read-line)
        vm' (execute-command (drop-outputs vm) command)]
    (print-output vm')
    (recur vm')))

(defn -main [& args]
  (let [intcode-program (read-intcode-program-from-file "resources/day_25/input.txt")
        vm (-> intcode-program
               (create-intcode-vm :inputs [] :memory-size 8000)
               run-until-needs-input)]
    (print-output vm)
    (repl vm)))
