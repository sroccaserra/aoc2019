(ns day-03.path-test
  (:require [clojure.test :refer :all]
            [day-03.path :refer :all]))

(def path-for-R2-U4
  [origin (point 1 0) (point 2 0) (point 2 1) (point 2 2) (point 2 3) (point 2 4)])
(def path-for-U2-R4
  [origin (point 0 1) (point 0 2) (point 1 2) (point 2 2) (point 3 2) (point 4 2)])

(deftest evaluating-command-tests
  (testing "evaluating commands from start position"
    (are [commands path] (= path
                            (add-command-points empty-path commands))
         '(U 1) [origin (point 0 1)]
         '(U 2) [origin (point 0 1) (point 0 2)]
         '(D 2) [origin (point 0 -1) (point 0 -2)]
         '(L 2) [origin (point -1 0) (point -2 0)]
         '(R 2) [origin (point 1 0) (point 2 0)]))

  (testing "evaluating a list of commands"
    (is (= path-for-R2-U4
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
    (is (= #{(point 2 2)}
           (path-intersections path-for-U2-R4 path-for-R2-U4)))))

(deftest testing-manhattan-distance
  (testing "simple case"
    (is (= 6 (manhattan-distance (point -3 3))))))

(deftest finding-closest-intersection
  (testing "finding intersections of first example"
    (let [path-1 (compute-all-points '((R 75) (D 30) (R 83) (U 83) (L 12) (D 49) (R 71) (U 7) (L 72)))
          path-2 (compute-all-points '((U 62) (R 66) (U 55) (R 34) (D 71) (R 55) (D 58) (R 83)))]
      (is (= 159
             (find-closest-intersection path-1 path-2)))))

  (testing "finding intersections of second example"
    (let [path-1 (compute-all-points '((R 98) (U 47) (R 26) (D 63) (R 33) (U 87) (L 62) (D 20) (R 33) (U 53) (R 51)))
          path-2 (compute-all-points '((U 98) (R 91) (D 20) (R 16) (D 67) (R 40) (U 7) (R 15) (U 6) (R 7)))]
      (is (= 135
             (find-closest-intersection path-1 path-2))))))

(deftest finding-shortest-wire-intersection
  (testing "finding intersections of first example"
    (let [path-1 (compute-all-points '((R 75) (D 30) (R 83) (U 83) (L 12) (D 49) (R 71) (U 7) (L 72)))
          path-2 (compute-all-points '((U 62) (R 66) (U 55) (R 34) (D 71) (R 55) (D 58) (R 83)))]
      (is (= 610
             (find-shortest-wire-intersection path-1 path-2)))))

  (testing "finding intersections of second example"
    (let [path-1 (compute-all-points '((R 98) (U 47) (R 26) (D 63) (R 33) (U 87) (L 62) (D 20) (R 33) (U 53) (R 51)))
          path-2 (compute-all-points '((U 98) (R 91) (D 20) (R 16) (D 67) (R 40) (U 7) (R 15) (U 6) (R 7)))]
      (is (= 410
             (find-shortest-wire-intersection path-1 path-2))))))
