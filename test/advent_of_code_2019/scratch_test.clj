(ns advent-of-code-2019.scratch-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2019.scratch :refer :all]))

(deftest scratch-tests
  (testing "salut renvoie sa valeur"
    (is (= 5 (salut 5)))))
