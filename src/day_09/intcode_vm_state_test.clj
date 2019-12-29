(ns day-09.intcode-vm-state-test
  (:require [clojure.test :refer :all]
            [day-09.intcode-vm-state :refer :all]))

(deftest parsing-first-instruction-value
  (testing "extracting opcode"
    (are [opcode first-value]
         (= opcode (-> first-value
                       parse-first-instruction-value
                       :opcode))
         1 1
         12 12
         23 123))

  (testing "extracting parameter modes"
    (are [modes first-value]
         (= modes (-> first-value
                      parse-first-instruction-value
                      :parameter-modes))
         [0 0 0] 1
         [0 0 0] 12
         [1 0 0] 123
         [0 1 0] 1023
         [0 0 1] 10023)))

(deftest inputs
  (testing "adding input values"
    (let [program [1 0 0 0 99]]
      (is (= [1 2 3] (-> program
                         create-intcode-vm
                         (add-input 1)
                         (add-input 1)
                         drop-input
                         (add-input 2)
                         (add-input 3)
                         :inputs))))))
