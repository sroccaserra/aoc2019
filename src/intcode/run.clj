(ns intcode.run
  (:require [intcode.vm-state :refer [halted?]]
            [intcode.instruction
             :refer [read-instruction execute-instruction]]))

(defn step [vm-state]
  (-> vm-state
      read-instruction
      (execute-instruction vm-state)))

(defn run [vm-state]
  (if (halted? vm-state)
    vm-state
    (recur (step vm-state))))
