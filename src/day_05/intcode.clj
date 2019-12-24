(ns day-05.intcode
  (:require [day-05.vm-state :refer [halted?]]
            [day-05.instruction :refer [read-instruction execute-instruction]]))

(defn create-intcode-vm [program & [input]]
  {:memory program
   :pc 0
   :input input
   :output []})

(defn step [vm-state]
  (-> vm-state
      read-instruction
      (execute-instruction vm-state)))

(defn run [vm-state]
  (if (halted?  vm-state)
    vm-state
    (recur (step vm-state))))
