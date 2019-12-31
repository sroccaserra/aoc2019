(ns aoc-common-cli-test
  (:require [clojure.test :refer :all]
            [aoc-common-cli :refer :all]))

(deftest reading-ints-from-input
  (testing "parsing a string of coma-separated ints"
    (is (= [1 2 99] (read-ints-from-line "1,2,99")))))
