(ns day-03.parser-test
  (:require [clojure.test :refer :all]
            [day-03.parser :refer :all]))

(deftest parsing-commands
  (testing "parsing R8 and U10 commands"
    (is (= '(R 8) (parse-command "R8")))
    (is (= '(U 10) (parse-command "U10"))))

  (testing "parsing several commands"
    (is (= '((R 3) (U 15))
           (parse-commands "R3,U15")))))
