(ns day-03.path-test
  (:require [clojure.test :refer :all]
            [day-03.path :refer :all]))

(deftest evaluating-command-tests
  (testing "evaluating commands from start position"
    (is (= [origin (point 0 1)]
           (add-command-points empty-path '(U 1))))

    (is (= [origin (point 0 1) (point 0 2)]
           (add-command-points empty-path '(U 2))))

    (is (= [origin (point 0 -1) (point 0 -2)]
           (add-command-points empty-path '(D 2))))

    (is (= [origin (point -1 0) (point -2 0)]
           (add-command-points empty-path '(L 2))))

    (is (= [origin (point 1 0) (point 2 0)]
           (add-command-points empty-path '(R 2)))))

  (testing "evaluating a list of commands"
    (is (= [origin (point 1 0) (point 2 0)
            (point 2 1) (point 2 2) (point 2 3) (point 2 4)]
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
    (let [path-1 [origin (point 0 1) (point 0 2)
                  (point 1 2) (point 2 2) (point 3 2) (point 4 2)]
          path-2 [origin (point 1 0) (point 2 0)
                  (point 2 1) (point 2 2) (point 2 3) (point 2 4)]]
      (is (= #{(point 2 2)}
             (path-intersections path-1 path-2))))))

(deftest testing-manhattan-distance
  (testing "simple case"
    (is (= 6 (manhattan-distance (point -3 3))))))
