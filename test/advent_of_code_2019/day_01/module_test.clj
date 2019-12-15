(ns advent-of-code-2019.day-01.module-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2019.day-01.module :refer :all]))

(deftest computing-fuel-for-mass
  (testing "fuel for mass 12"
    (is (= 2 (fuel-for-mass 12))))

  (testing "fuel for mass 14"
    (is (= 2 (fuel-for-mass 14))))

  (testing "fuel for mass 1969"
    (is (= 654 (fuel-for-mass 1969))))

  (testing "fuel for mass 100756"
    (is (= 33583 (fuel-for-mass 100756)))))

(deftest computing-fuel-for-one-module
  (testing "fuel for module of mass 14"
    (is (= 2 (fuel-for-module-mass 14))))

  (testing "fuel for module of mass 1969"
    (is (= 966 (fuel-for-module-mass 1969))))

  (testing "fuel for module of mass 100756"
    (is (= 50346 (fuel-for-module-mass 100756)))))

(deftest computing-total-required-fuel
  (testing "required fuel for two modules"
    (is (= 4 (compute-required-fuel-for-module-masses [12 14]))))

  (testing "required fuel for three modules"
    (is (= 970 (compute-required-fuel-for-module-masses [12 14 1969])))))
