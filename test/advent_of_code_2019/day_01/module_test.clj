(ns advent-of-code-2019.day-01.module-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2019.day-01.module :refer :all]))


(deftest compute-fuel-mass
  (testing "fuel for module of mass 12"
    (is (= 2 (fuel-for-module 12))))

  (testing "fuel for module of mass 14"
    (is (= 2 (fuel-for-module 14))))

  (testing "fuel for module of mass 1969"
    (is (= 654 (fuel-for-module 1969))))

  (testing "fuel for module of mass 100756"
    (is (= 33583 (fuel-for-module 100756)))))
