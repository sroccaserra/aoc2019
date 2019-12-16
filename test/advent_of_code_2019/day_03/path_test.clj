(ns advent-of-code-2019.day-03.path-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2019.day-03.path :refer :all]))

(deftest command-tests
  (testing "parsing R8 and U5 commands"
    (is (= ['R 8] (parse-command "R8")))
    (is (= ['U 5] (parse-command "U5"))))

  (testing "evaluating commands from start position"
    (is (= '([0 1] [0 0])
           (eval-command ['U 1] '([0 0]))))

    (is (= '([0 2] [0 1] [0 0])
           (eval-command ['U 2] '([0 0]))))

    (is (= '([0 -2] [0 -1] [0 0])
           (eval-command ['D 2] '([0 0]))))

    (is (= '([-2 0] [-1 0] [0 0])
           (eval-command ['L 2] '([0 0]))))

    (is (= '([2 0] [1 0] [0 0])
           (eval-command ['R 2] '([0 0]))))))
