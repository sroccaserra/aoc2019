(ns day-09.intcode-instruction-test
  (:require [clojure.test :refer :all]
            [day-09.intcode-vm-state :refer [create-intcode-vm adjust-relative-base]]
            [day-09.intcode-instruction :refer :all]))

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
    (let [vm (create-intcode-vm [3 0 99] :inputs [77])]
      (is (= (->InputInstruction 0)
             (read-instruction vm)))))

  (testing "reading input relative mode"
    (let [vm (-> [203 0 99 0]
                 (create-intcode-vm :inputs [77])
                 (adjust-relative-base 3))]
      (is (= (->InputInstruction 3)
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
      (is (= (->ComparisonInstruction < 1 3 0)
             (read-instruction vm))))))
