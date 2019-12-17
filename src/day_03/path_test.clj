(ns day-03.path-test
  (:require [clojure.test :refer :all]
            [day-03.path :refer :all]))

(deftest evaluating-command-tests
  (testing "evaluating commands from start position"
    (is (= [[0 0] [0 1]]
           (eval-command empty-path '(U 1))))

    (is (= [[0 0] [0 2]]
           (eval-command empty-path '(U 2))))

    (is (= [[0 0] [0 -2]]
           (eval-command empty-path '(D 2))))

    (is (= [[0 0] [-2 0]]
           (eval-command empty-path '(L 2))))

    (is (= [[0 0] [2 0]]
           (eval-command empty-path '(R 2)))))

  (testing "evaluating a list of commands"
    (is (= [[0 0] [0 1] [1 1]]
           (compute-path '((U 1) (R 1)))))))

(deftest computing-complex-paths
  (testing "computing a few long paths"
    (is (= 4
           (count (compute-path '((R 990) (U 796) (U 604))))))))

(deftest find-segment-intersections
  (testing "building all points for vertical segments"
    (is (= #{[0 0] [0 1]}
           (all-points-between [[0 0] [0 1]])))
    (is (= #{[0 0] [0 1] [0 2]}
           (all-points-between [[0 0] [0 2]]))))

  (testing "building all points for horizontal segments"
    (is (= #{[0 0] [1 0]}
           (all-points-between [[0 0] [1 0]])))
    (is (= #{[0 0] [1 0] [2 0]}
           (all-points-between [[0 0] [2 0]]))))

  (testing "finding intersection of two segments"
    "First:
    ......
    .+....
    .|....
    .X-+..
    ......

    Then:
    .......
    ...+...
    ...|...
    .+-X-+.
    ...|...
    .o.+...
    ......."
    (is (= #{[0 0]}
           (segment-intersection [[0 0] [2 0]]
                                 [[0 0] [0 2]])))
    (is (= #{[2 2]}
           (segment-intersection [[2 0] [2 4]]
                                 [[0 2] [4 2]])))))

(deftest finding-segments-in-path
  (testing "first example"
    "Finding:
    .......
    ...+...
    ...|...
    ...|...
    ...|...
    .o-+...
    ......."
    (let [path ['(0 0) '(2 0) '(2 4)]]
      (is (= [[[0 0] [2 0]] [[2 0] [2 4]]]
             (compute-path-segments path)))))

  (testing "second example"
    "Finding:
    .......
    .......
    .......
    .+---+.
    .|.....
    .o.....
    ......."
    (let [path ['(0 0) '(0 2) '(4 2)]]
      (is (= [[[0 0] [0 2]] [[0 2] [4 2]]]
             (compute-path-segments path))))))

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
    (let [path-1 ['(0 0) '(0 2) '(4 2)]
          path-2 ['(0 0) '(2 0) '(2 4)]]
      (is (= #{[2 2]}
             (path-intersections path-1 path-2))))))

(deftest testing-manhattan-distance
  (testing "simple case"
    (is (= 6 (manhattan-distance [-3 3])))))
