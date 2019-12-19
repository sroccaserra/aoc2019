(ns day-03.path-test
  (:require [clojure.test :refer :all]
            [day-03.path :refer :all]))

(deftest evaluating-command-tests
  (testing "evaluating commands from start position"
    (is (= [[0 0] [0 1]]
           (add-command-points empty-path '(U 1))))

    (is (= [[0 0] [0 1] [0 2]]
           (add-command-points empty-path '(U 2))))

    (is (= [[0 0] [0 -1] [0 -2]]
           (add-command-points empty-path '(D 2))))

    (is (= [[0 0] [-1 0] [-2 0]]
           (add-command-points empty-path '(L 2))))

    (is (= [[0 0] [1 0] [2 0]]
           (add-command-points empty-path '(R 2)))))

  (testing "evaluating a list of commands"
    (is (= [[0 0] [1 0] [2 0] [2 1] [2 2] [2 3] [2 4]]
           (compute-all-points '((R 2) (U 4))))))

  (testing "computing a few long commands"
    (is (= 2391
           (count (compute-all-points '((R 990) (U 796) (U 604))))))))

(deftest finding-intersections-of-paths
  (testing "finding intersections of paths"
    "Finding:
    .......
    ...+...
    ...|...
    .+-X-+.
    .|.|...
    .o-+...
    ......."
    (let [path-1 [[0 0] [0 1] [0 2] [1 2] [2 2] [3 2] [4 2]]
          path-2 [[0 0] [1 0] [2 0] [2 1] [2 2] [2 3] [2 4]]]
      (is (= #{[2 2]}
             (path-intersections path-1 path-2))))))

(deftest testing-manhattan-distance
  (testing "simple case"
    (is (= 6 (manhattan-distance [-3 3])))))
