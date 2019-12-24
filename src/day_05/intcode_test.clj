(ns day-05.intcode-test
  (:require [clojure.test :refer :all]
            [day-05.intcode :refer :all]))

(def simple-program [1 0 0 0 99])

(deftest intcode-vm
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
      (is (= [-2 0 5 0 99 -3] (:memory vm)))))

  (testing "step through a 1 - 3 immediate addition"
    (let [vm (-> [1101 1 -3 0 99]
                 create-intcode-vm
                 step)]
      (is (= [-2 1 -3 0 99] (:memory vm))))))

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
      (is (= [30 1 1 4 2 5 6 0 99] (:memory vm)))))

  (testing "run an immediate mode program"
    (let [vm (-> [1101 100 -1 4 0]
                 create-intcode-vm
                 run)]
      (is (= [1101 100 -1 4 99] (:memory vm))))))

(deftest restoring-state
  (testing "restoring 1202 program alarm"
    (let [vm (-> [1 0 0 0 99 0 0 0 0 0 0 0 0]
                 create-intcode-vm
                 (restore-state 12 2)
                 run)]
      (is (= [2 12 2 0 99 0 0 0 0 0 0 0 0] (:memory vm))))))

(deftest input-and-output
  (testing "reading input"
    (let [vm (-> (create-intcode-vm [3 0 99] 77)
                 step)]
      (is (= [77 0 99] (:memory vm)))
      (is (= 2 (:pc vm)))))

  (testing "writing-output"
    (let [vm (-> (create-intcode-vm [4 5 4 6 99 77 88])
                 run)]
      (is (= [77 88] (:output vm)))))

  (testing "writing the input"
    (let [output (-> (create-intcode-vm [3 0 4 0 99] 77)
                     run
                     :output)]
      (is (= [77]
             output)))))

(deftest jumping
  (testing "jump immediate mode"
    (let [vm (-> (create-intcode-vm [1105 1 7 1 0 0 0 99])
                 run)]
      (is (= 7
             (:pc vm)))
      (is (= 1105
             (get-in vm [:memory 0])))))

  (testing "no jump immediate mode"
    (let [vm (-> (create-intcode-vm [1105 0 7 1 0 0 0 99])
                 run)]
      (is (= 7
             (:pc vm)))
      (is (= (+ 1105 1105)
             (get-in vm [:memory 0]))))))
