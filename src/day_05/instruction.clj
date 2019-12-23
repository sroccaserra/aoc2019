(ns day-05.instruction
  (:require [day-05.vm-state :refer :all]))

(defprotocol Instruction
  (length [_])
  (execute-instruction [this vm-state]))

;; Halt

(defrecord HaltInstruction []
  Instruction
  (execute-instruction [_ vm-state]))

(defn create-halt-instruction [_]
  (->HaltInstruction))

;; Add

(defrecord AddInstruction [parameter-1 parameter-2 dest]
  Instruction
  (length [_] 4)
  (execute-instruction [this vm-state]
    (-> vm-state
        (write-int-at dest (+ parameter-1 parameter-2))
        (increment-pc (length this)))))

(defn create-add-instruction [vm-state]
  (->AddInstruction (parameter-value vm-state 1)
                    (parameter-value vm-state 2)
                    (parameter-address vm-state 3)))

;; Mul

(defrecord MulInstruction [parameter-1 parameter-2 dest]
  Instruction
  (length [_] 4)
  (execute-instruction [this vm-state]
    (-> vm-state
        (write-int-at dest (* parameter-1 parameter-2))
        (increment-pc (length this)))))

(defn create-mul-instruction [vm-state]
  (->MulInstruction (parameter-value vm-state 1)
                    (parameter-value vm-state 2)
                    (parameter-address vm-state 3)))

;; Read instructions

(def instruction-fn {halt-opcode create-halt-instruction
                     1 create-add-instruction
                     2 create-mul-instruction})

(defn read-instruction [vm-state]
  (let [opcode (read-opcode vm-state)
        create-instruction (instruction-fn opcode)]
    (create-instruction vm-state)))
