(ns advent-of-code-2019.day-01.main-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2019.day-01.main :refer :all]))


(deftest parsing
  (testing "parsing a seq of lines"
    (is (= [1 2 3]  (parse-lines-as-integers ["1" "2" "3"])))))
