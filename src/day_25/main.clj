(ns day-25.main
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-common-cli :refer [read-intcode-program-from-file]]
            [intcode.vm-state :refer [create-intcode-vm add-inputs drop-outputs]]
            [intcode.run :refer [run-until-needs-input]]))

(def commands ["south" "west" "take shell"
               "east" "east" "take space heater"
               "west" "north" "west" "north" "take jam"
               "east" "south" "take asterisk"
               "south" "take klein bottle"
               "east" "take spool of cat6"
               "west" "north" "north" "west" "north" "take astronaut ice cream"
               "north" "east" "south" "take space law space brochure"
               "inv"
               "north" "west" "south" "south" "south" "south" "west"])

(defn encode-command [command]
  (concat (map int command) [10]))

(defn execute-command [vm command]
  (run-until-needs-input (add-inputs vm (encode-command command))))

(defn decode-output [output]
  (apply str (map char output)))

(defn print-output [vm]
  (println (decode-output (:outputs vm))))

(defn eval-print-command [vm command]
  (let [vm' (execute-command (drop-outputs vm) command)]
    (print-output vm')
    vm'))

(defn repl [vm]
  (let [command (read-line)]
    (recur (eval-print-command vm command))))

(defn -main [& args]
  (let [intcode-program (read-intcode-program-from-file "resources/day_25/input.txt")
        vm (-> intcode-program
               (create-intcode-vm :inputs [] :memory-size 8000)
               run-until-needs-input)]
    (print-output vm)
    (repl (reduce (fn [vm command] (println command) (eval-print-command vm command))
                  vm commands))))
