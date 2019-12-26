(ns day-07.intcode-vm-state-test
  (:require [clojure.test :refer :all]
            [day-07.intcode-vm-state :refer :all]))

(deftest parsing-first-instruction-value
  (testing "extracting opcode"
    (are [first-value opcode]
         (= opcode (:opcode (parse-first-instruction-value first-value)))
         1 1
         12 12
         123 23))
  (testing "extracting parameter modes"
    (are [first-value modes]
         (= modes (:parameter-modes
                    (parse-first-instruction-value first-value)))
         1 [0 0 0]
         12 [0 0 0]
         123 [1 0 0])))
