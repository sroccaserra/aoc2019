(ns day-05.instruction
  (:require [day-05.vm-state :refer :all]))

(defprotocol Instruction
  (execute-instruction [_ vm-state]))

;; Halt

(defrecord HaltInstruction []
  Instruction
  (execute-instruction [_ vm-state]))

(defn create-halt-instruction [_]
  (->HaltInstruction))

;; Add

(defrecord AddInstruction [parameter-1 parameter-2 dest]
  Instruction
  (execute-instruction [_ vm-state]
    (-> vm-state
        (assoc-in [:memory dest] (+ parameter-1 parameter-2))
        (update-in [:pc] + 4))))

(defn create-add-instruction [vm-state]
  (->AddInstruction (parameter-value vm-state 1)
                    (parameter-value vm-state 2)
                    (parameter-address vm-state 3)))

;; Mul

(defrecord MulInstruction [parameter-1 parameter-2 dest]
  Instruction
  (execute-instruction [_ vm-state]
    (-> vm-state
        (assoc-in [:memory dest] (* parameter-1 parameter-2))
        (update-in [:pc] + 4))))

(defn create-mul-instruction [vm-state]
  (->MulInstruction (parameter-value vm-state 1)
                    (parameter-value vm-state 2)
                    (parameter-address vm-state 3)))

;; Read instruction

(def instruction-for-opcode {halt-opcode create-halt-instruction
                             1 create-add-instruction
                             2 create-mul-instruction})

(defn read-instruction [vm-state]
  (let [opcode (read-opcode vm-state)]
    ((instruction-for-opcode opcode) vm-state)))
