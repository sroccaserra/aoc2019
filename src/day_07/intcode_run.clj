(ns day-07.intcode-run
  (:require [day-07.intcode-vm-state :refer [halted?]]
            [day-07.intcode-instruction
             :refer [read-instruction execute-instruction]]))

(defn step [vm-state]
  (-> vm-state
      read-instruction
      (execute-instruction vm-state)))

(defn run [vm-state]
  (if (halted?  vm-state)
    vm-state
    (recur (step vm-state))))
