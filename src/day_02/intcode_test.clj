(ns day-02.intcode-test
  (:require [clojure.test :refer :all]
            [day-02.intcode :refer :all]))

(deftest intcode-vm
  (testing "find opcode"
    (let [vm (create-intcode-vm [99])]
      (is (= 99 (opcode vm)))))

  (testing "addresses of parameters"
    (let [vm (create-intcode-vm [1 2 4 0 99])]
      (is (= 2 (parameter-1-address vm)))
      (is (= 4 (parameter-2-address vm)))
      (is (= 0 (parameter-3-address vm)))))

  (testing "instruction values"
    (let [vm (create-intcode-vm [1 2 4 0 99])]
      (is (= [4 99] (instruction-values vm)))))

  (testing "program counter increment"
    (let [vm (-> [1 0 0 0 99]
                 create-intcode-vm
                 step)]
      (is (= 99 (opcode vm))))))

(deftest intcode-addition
  (testing "step through a 1 + 1 addition"
    (let [vm (-> [1 0 0 0 99]
                 create-intcode-vm
                 step)]
      (is (= 99 (opcode vm)))
      (is (= [2 0 0 0 99] (:memory vm)))))

  (testing "step through a 2 + 2 addition"
    (let [vm (-> [2 0 0 0 99]
                 create-intcode-vm
                 step)]
      (is (= [4 0 0 0 99] (:memory vm))))))

(deftest intcode-multiplication
  (testing "step through a 3 * 2 multiplication"
    (let [vm (-> [2 3 0 3 99]
                 create-intcode-vm
                 step)]
      (is (= [2 3 0 6 99] (:memory vm)))))

  (testing "step through a 99 * 99 multiplication"
    (let [vm (-> [2 4 4 5 99 0]
                 create-intcode-vm
                 step)]
      (is (= [2 4 4 5 99 9801] (:memory vm))))))

(deftest intcode-multi-step-program
  (testing "run a two step program"
    (let [vm (-> [1 1 1 4 99 5 6 0 99]
                 create-intcode-vm
                 run)]
      (is (= [30 1 1 4 2 5 6 0 99] (:memory vm))))))

(deftest restoring-state
  (testing "restoring 1202 program alarm"
    (let [vm (-> [1 0 0 0 99 0 0 0 0 0 0 0 0]
                 create-intcode-vm
                 (restore-state 12 2)
                 run)]
      (is (= [2 12 2 0 99 0 0 0 0 0 0 0 0] (:memory vm))))))
