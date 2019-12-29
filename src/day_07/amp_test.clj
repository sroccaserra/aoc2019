(ns day-07.amp-test
  (:require [clojure.test :refer :all]
            [day-07.intcode-vm-state :refer [create-intcode-vm halted?]]
            [day-07.amp :refer :all]))

(deftest amp-chains-short-programs
  (testing "first example program"
    (is (= 43210 (run-amp-chain [3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0]
                                [4 3 2 1 0]
                                0))))

  (testing "second example program"
    (is (= 54321 (run-amp-chain [3 23 3 24 1002 24 10 24 1002 23 -1 23  101 5 23 23 1 24 23 23 4 23 99 0 0]
                                [0 1 2 3 4]
                                0))))

  (testing "third example program"
    (is (= 65210 (run-amp-chain [3 31 3 32 1002 32 10 32 1001 31 -2 31 1007 31 0 33  1002 33 7 33 1 33 31 31 1 32 31 31 4 31 99 0 0 0]
                                [1 0 4 3 2]
                                0)))))

(deftest amp-running-until-needs-input
  (testing "needing input from the start"
    (let [vm (create-intcode-vm [3 0 99])]
      (is (= vm (run-until-need-input vm)))))

  (testing "should run to completion if not needing input"
    (let [vm (create-intcode-vm [3 0 99] :inputs [77])]
      (is (halted? (run-until-need-input vm))))))

(deftest running-amp-loop
  (is (nil? (run-amp-loop [3 0 3 1 99] [5 6 7 8 9] 0))))

(deftest amp-chains-short-programs
  (testing "first example program"
    (is (= 139629729
           (run-amp-loop [3 26 1001 26 -4 26 3 27 1002 27 2 27 1 27 26 27 4 27 1001 28 -1 28 1005 28 6 99 0 0 5]
                         [9 8 7 6 5]
                         0)))))
