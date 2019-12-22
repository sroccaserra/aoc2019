(ns day-05.intcode-test
  (:require [clojure.test :refer :all]
            [day-05.intcode :refer :all]))

(def simple-program [1 0 0 0 99])

(deftest intcode-vm
  (testing "find opcode"
    (let [vm (create-intcode-vm [99])]
      (is (= {:opcode 99}
             (read-instruction vm)))))

  (testing "read an instruction"
    (let [vm (create-intcode-vm [1 2 4 0 99])]
      (is (= {:opcode 1 :size 4 :value-1 4 :value-2 99 :dest 0}
             (read-instruction vm)))))

  (testing "program counter increment"
    (let [vm (-> simple-program
                 create-intcode-vm
                 step)]
      (is (= 4 (:pc vm))))))

(deftest intcode-addition
  (testing "step through a 1 + 1 addition"
    (let [vm (-> simple-program
                 create-intcode-vm
                 step)]
      (is (= [2 0 0 0 99] (:memory vm)))))

  (testing "step through a 1 + 2 addition"
    (let [vm (-> [1 0 5 0 99 2]
                 create-intcode-vm
                 step)]
      (is (= [3 0 5 0 99 2] (:memory vm)))))

  (testing "step through a 1 - 3 addition"
    (let [vm (-> [1 0 5 0 99 -3]
                 create-intcode-vm
                 step)]
      (is (= [-2 0 5 0 99 -3] (:memory vm))))))

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
