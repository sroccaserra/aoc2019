(ns day-07.intcode-instruction
  (:require [day-07.intcode-vm-state :refer :all]))

(defprotocol Instruction
  (length [this])
  (execute-instruction [this vm-state]))

;; Halt

(defrecord HaltInstruction []
  Instruction
  (execute-instruction [_ _]
    (throw (AssertionError. "Halt instructions should not be executed."))))

(defn create-halt-instruction [_ _]
  (->HaltInstruction))

;; Add & Mul

(defrecord MathInstruction [operation parameter-1 parameter-2 dest]
  Instruction
  (length [_] 4)
  (execute-instruction [this vm-state]
    (-> vm-state
        (write-int-at dest (operation parameter-1 parameter-2))
        (increment-pc (length this)))))

(defn create-math-instruction [operation vm-state parameter-modes]
  (let [parameter-1 (if (= 1 (get parameter-modes 0))
                      (parameter-value vm-state 1)
                      (parameter-value-indirect vm-state 1))
        parameter-2 (if (= 1 (get parameter-modes 1))
                      (parameter-value vm-state 2)
                      (parameter-value-indirect vm-state 2))]
    (->MathInstruction operation parameter-1 parameter-2
                       (parameter-value vm-state 3))))

;; Input instruction

(defrecord InputInstruction [dest]
  Instruction
  (length [_] 2)
  (execute-instruction [this vm-state]
    (-> vm-state
        (write-int-at dest (read-input vm-state))
        (increment-pc (length this)))))

(defn create-input-instruction [vm-state _]
  (->InputInstruction (parameter-value vm-state 1)))

;; Output instruction

(defrecord OutputInstruction [output-value]
  Instruction
  (length [_] 2)
  (execute-instruction [this vm-state]
    (-> vm-state
        (add-output-value output-value)
        (increment-pc (length this)))))

(defn create-output-instruction [vm-state _]
  (->OutputInstruction (parameter-value-indirect vm-state 1)))

;; Jumping instructions

(defrecord JumpInstruction [jump-test parameter-1 parameter-2]
  Instruction
  (length [_] 3)
  (execute-instruction [this vm-state]
    (if (jump-test parameter-1)
      (increment-pc vm-state (length this))
      (set-pc vm-state parameter-2))))

(defn create-jump-instruction [jump-test vm-state parameter-modes]
  (let [parameter-1 (if (= 1 (get parameter-modes 0))
                      (parameter-value vm-state 1)
                      (parameter-value-indirect vm-state 1))
        parameter-2 (if (= 1 (get parameter-modes 1))
                      (parameter-value vm-state 2)
                      (parameter-value-indirect vm-state 2))]
    (->JumpInstruction jump-test parameter-1 parameter-2)))

;; Comparison instructions

(defrecord ComparisonInstruction [comparison-test parameter-1 parameter-2 dest]
  Instruction
  (length [_] 4)
  (execute-instruction [this vm-state]
    (-> vm-state
        (write-int-at dest (if (comparison-test parameter-1 parameter-2) 1 0))
        (increment-pc (length this)))))

(defn create-comparison-instruction [comparison-test vm-state parameter-modes]
  (let [parameter-1 (if (= 1 (get parameter-modes 0))
                      (parameter-value vm-state 1)
                      (parameter-value-indirect vm-state 1))
        parameter-2 (if (= 1 (get parameter-modes 1))
                      (parameter-value vm-state 2)
                      (parameter-value-indirect vm-state 2))]
    (->ComparisonInstruction comparison-test
                             parameter-1
                             parameter-2
                             (parameter-value vm-state 3))))


;; Reading instructions

(def not-zero? (comp not zero?))

(def instruction-fn {halt-opcode create-halt-instruction
                     1 (partial create-math-instruction +)
                     2 (partial create-math-instruction *)
                     3 create-input-instruction
                     4 create-output-instruction
                     5 (partial create-jump-instruction zero?)
                     6 (partial create-jump-instruction not-zero?)
                     7 (partial create-comparison-instruction <)
                     8 (partial create-comparison-instruction =)})

(defn read-instruction [vm-state]
  (let [{:keys [opcode parameter-modes]} (-> vm-state
                                             read-first-instruction-value
                                             parse-first-instruction-value)
        create-instruction (get instruction-fn opcode)]
    (create-instruction vm-state parameter-modes)))
