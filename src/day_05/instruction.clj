(ns day-05.instruction
  (:require [day-05.vm-state :refer :all]))

(defprotocol Instruction
  (length [this])
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
  (->AddInstruction (parameter-value-indirect vm-state 1)
                    (parameter-value-indirect vm-state 2)
                    (parameter-value vm-state 3)))

;; Mul

(defrecord MulInstruction [parameter-1 parameter-2 dest]
  Instruction
  (length [_] 4)
  (execute-instruction [this vm-state]
    (-> vm-state
        (write-int-at dest (* parameter-1 parameter-2))
        (increment-pc (length this)))))

(defn create-mul-instruction [vm-state]
  (->MulInstruction (parameter-value-indirect vm-state 1)
                    (parameter-value-indirect vm-state 2)
                    (parameter-value vm-state 3)))

;; Input instruction

(defrecord InputInstruction [dest]
  Instruction
  (length [_] 2)
  (execute-instruction [this vm-state]
    (-> vm-state
        (write-int-at dest (read-input vm-state))
        (increment-pc (length this)))))

(defn create-input-instruction [vm-state]
  (->InputInstruction (parameter-value vm-state 1)))

;; Output instruction

(defrecord OutputInstruction [output-value]
  Instruction
  (length [_] 2)
  (execute-instruction [this vm-state]
    (-> vm-state
        (add-output-value output-value)
        (increment-pc (length this)))))

(defn create-output-instruction [vm-state]
  (->OutputInstruction (parameter-value vm-state 1)))

;; Reading instructions

(def instruction-fn {halt-opcode create-halt-instruction
                     1 create-add-instruction
                     2 create-mul-instruction
                     3 create-input-instruction
                     4 create-output-instruction})

(defn read-instruction [vm-state]
  (let [{opcode :opcode} (-> vm-state
                             read-first-instruction-value
                             parse-first-instruction-value)
        create-instruction (get instruction-fn opcode)]
    (create-instruction vm-state)))
