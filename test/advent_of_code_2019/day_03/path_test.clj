(ns advent-of-code-2019.day-03.path-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2019.day-03.path :refer :all]))

(deftest command-tests
  (testing "parsing R8 and U10 commands"
    (is (= ['R 8] (parse-command "R8")))
    (is (= ['U 10] (parse-command "U10"))))

  (testing "evaluating commands from start position"
    (is (= '([0 1] [0 0])
           (eval-command ['U 1] start-position)))

    (is (= '([0 2] [0 1] [0 0])
           (eval-command ['U 2] start-position)))

    (is (= '([0 -2] [0 -1] [0 0])
           (eval-command ['D 2] start-position)))

    (is (= '([-2 0] [-1 0] [0 0])
           (eval-command ['L 2] start-position)))

    (is (= '([2 0] [1 0] [0 0])
           (eval-command ['R 2] start-position))))

  (testing "evaluating a list of commands"
    (is (= '([1 1] [0 1] [0 0])
           (compute-path ['U 1 'R 1])))))
