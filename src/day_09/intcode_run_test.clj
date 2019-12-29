(ns day-09.intcode-run-test
  (:require [clojure.test :refer :all]
            [day-09.intcode-vm-state :refer [create-intcode-vm add-input]]
            [day-09.intcode-run :refer :all]))

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

(deftest input-and-output
  (testing "reading input"
    (let [vm (-> (create-intcode-vm [3 0 99]
                                    :inputs [77])
                 step)]
      (is (= [77 0 99] (:memory vm)))
      (is (= [] (:inputs vm)))
      (is (= 2 (:pc vm)))))

  (testing "writing-output"
    (let [vm (-> (create-intcode-vm [4 5 4 6 99 77 88])
                 run)]
      (is (= [77 88] (:outputs vm)))))

  (testing "writing the input"
    (let [outputs (-> (create-intcode-vm [3 0 4 0 99]
                                        :inputs [77])
                     run
                     :outputs)]
      (is (= [77]
             outputs)))))

(deftest jumping
  (testing "jz immediate mode"
    (let [vm (-> (create-intcode-vm [1105 1 7 1 0 0 0 99])
                 run)]
      (is (= 7
             (:pc vm)))
      (is (= 1105
             (get-in vm [:memory 0])))))

  (testing "no jz immediate mode"
    (let [vm (-> (create-intcode-vm [1105 0 7 1 0 0 0 99])
                 run)]
      (is (= 7
             (:pc vm)))
      (is (= (+ 1105 1105)
             (get-in vm [:memory 0])))))

  (testing "jnz immediate mode"
    (let [vm (-> (create-intcode-vm [1106 0 7 1 0 0 0 99])
                 run)]
      (is (= 7
             (:pc vm)))
      (is (= 1106
             (get-in vm [:memory 0])))))

  (testing "no jnz immediate mode"
    (let [vm (-> (create-intcode-vm [1106 1 7 1 0 0 0 99])
                 run)]
      (is (= 7
             (:pc vm)))
      (is (= (+ 1106 1106)
             (get-in vm [:memory 0]))))))

(deftest testing-complex-programs
  (testing "example program position mode"
    (let [vm (create-intcode-vm [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9])]
      (are [input-value outputs] (is (= outputs
                                        (-> vm
                                            (add-input input-value)
                                            run
                                            :outputs)))
           0 [0]
           1 [1]
           2 [1]
           -1 [1])))

  (testing "example program immediate mode"
    (let [vm (create-intcode-vm [3 3 1105 -1 9 1101 0 0 12 4 12 99 1])]
      (are [input-value outputs] (is (= outputs
                                        (-> vm
                                            (add-input input-value)
                                            run
                                            :outputs)))
           0 [0]
           1 [1]
           2 [1]
           -1 [1]))))

(deftest comparing
  (testing "less than 8 (position mode)"
    (let [vm (create-intcode-vm [3 9 7 9 10 9 4 9 99 -1 8])]
      (are [input-value outputs] (is (= outputs
                                        (-> vm
                                            (add-input input-value)
                                            run
                                            :outputs)))
           6 [1]
           7 [1]
           8 [0]
           9 [0])))

  (testing "less than 8 (immediate mode)"
    (let [vm (create-intcode-vm [3 3 1107 -1 8 3 4 3 99])]
      (are [input-value outputs] (is (= outputs
                                        (-> vm
                                            (add-input input-value)
                                            run
                                            :outputs)))
           6 [1]
           7 [1]
           8 [0]
           9 [0])))

  (testing "equals 8 (position mode)"
    (let [vm (create-intcode-vm [3 9 8 9 10 9 4 9 99 -1 8])]
      (are [input-value outputs] (is (= outputs
                                        (-> vm
                                            (add-input input-value)
                                            run
                                            :outputs)))
           7 [0]
           8 [1]
           9 [0])))

 (testing "equals 8 (immediate mode)"
    (let [vm (create-intcode-vm [3 3 1108 -1 8 3 4 3 99])]
      (are [input-value outputs] (is (= outputs
                                        (-> vm
                                            (add-input input-value)
                                            run
                                            :outputs)))
           7 [0]
           8 [1]
           9 [0]))))
