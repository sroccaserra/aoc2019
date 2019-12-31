(ns intcode.run
  (:require [intcode.vm-state :refer [halted? needs-input?]]
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

(defn run-until-needs-input [vm-state]
  (cond
    (halted? vm-state) vm-state
    (needs-input? vm-state) vm-state
    :else (recur (step vm-state))))
