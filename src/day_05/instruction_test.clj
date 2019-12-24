(ns day-05.instruction-test
  (:require [clojure.test :refer :all]
            [day-05.intcode :refer [create-intcode-vm]]
            [day-05.instruction :refer :all]))

(deftest reading-instructions
  (testing "find opcode"
    (let [vm (create-intcode-vm [99])]
      (is (= (->HaltInstruction)
             (read-instruction vm)))))

  (testing "read an add instruction"
    (let [vm (create-intcode-vm [1 2 4 0 99])]
      (is (= (->MathInstruction + 4 99 0)
             (read-instruction vm)))))

  (testing "read an add instruction with immediate mode"
    (let [vm (create-intcode-vm [1101 2 4 0 99])]
      (is (= (->MathInstruction + 2 4 0)
             (read-instruction vm)))))

  (testing "read a mul instruction"
    (let [vm (create-intcode-vm [2 2 4 0 99])]
      (is (= (->MathInstruction * 4 99 0)
             (read-instruction vm)))))

  (testing "read a mul instruction with mis-matched modes"
    (let [vm (create-intcode-vm [1002 3 4 0 99])]
      (is (= (->MathInstruction * 0 4 0)
             (read-instruction vm))))))

(deftest testing-input
  (testing "reading input"
    (let [vm (create-intcode-vm [3 0 99] 77)]
      (is (= (->InputInstruction 0)
             (read-instruction vm))))))

(deftest testing-jumps
  (testing "jumping if zero"
    (let [vm (create-intcode-vm [1105 1 3 99])]
      (is (= (->JumpInstruction zero? 1 3)
             (read-instruction vm)))))

  (testing "jumping if not zero"
    (let [vm (create-intcode-vm [1106 0 3 99])]
      (is (= (->JumpInstruction not-zero? 0 3)
             (read-instruction vm))))))

(deftest testing-comparisons
  (testing "less than"
    (let [vm (create-intcode-vm [1107 1 3 0 99])]
      (is (= (->LessThanInstruction 1 3 0)
             (read-instruction vm))))))
