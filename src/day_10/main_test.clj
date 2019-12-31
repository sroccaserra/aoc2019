(ns day-10.main-test
  (:require [clojure.test :refer :all]
            [day-10.main :refer :all]))

(def example-1 [".#..#"
                "....."
                "#####"
                "....#"
                "...##"])

(def example-letters ["#........."
                      "...A......"
                      "...B..a..."
                      ".EDCG....a"
                      "..F.c.b..."
                      ".....c...."
                      "..efd.c.gb"
                      ".......c.."
                      "....f...c."
                      "...e..d..c"])

(def example-2 ["......#.#."
                "#..#.#...."
                "..#######."
                ".#.#.###.."
                ".#..#....."
                "..#....#.#"
                "#..#....#."
                ".##.#..###"
                "##...#..#."
                ".#....####"])

(def example-3 ["#.#...#.#."
                ".###....#."
                ".#....#..."
                "##.#.#.#.#"
                "....#.#.#."
                ".##..###.#"
                "..#...##.."
                "..##....##"
                "......#..."
                ".####.###."])

(def example-4 [".#..#..###"
                "####.###.#"
                "....###.#."
                "..###.##.#"
                "##.##.#.#."
                "....###..#"
                "..#.#..#.#"
                "#..#.#.###"
                ".##...##.#"
                ".....#.#.."])

(def example-5 [".#..##.###...#######"
                "##.############..##."
                ".#.######.########.#"
                ".###.#######.####.#."
                "#####.##.#.##.###.##"
                "..#####..#.#########"
                "####################"
                "#.####....###.#.#.##"
                "##.#################"
                "#####.##.###..####.."
                "..######..##.#######"
                "####.##.####...##..#"
                ".#####..#.######.###"
                "##...#.##########..."
                "#.##########.#######"
                ".####.#.###.###.#.##"
                "....##.##.###..#####"
                ".#.#.###########.###"
                "#.#.#.#####.####.###"
                "###.##.####.##.#..##"])

(deftest testing-examples
  (are [example expected]
       (= expected
          (-> example
              parse-asteroid-positions
              find-best-location))
       example-1 {:x 3 :y 4 :nb-seen 8}
       example-2 {:x 5 :y 8 :nb-seen 33}
       example-3 {:x 1 :y 2 :nb-seen 35}
       example-4 {:x 6 :y 3 :nb-seen 41}
       example-5 {:x 11 :y 13 :nb-seen 210}))

(deftest change-coords-and-add-data
  (testing "changing coords"
    (is (= [{:x -1, :y -2} {:x 2, :y -2}
            {:x -2, :y 0} {:x -1, :y 0} {:x 0, :y 0} {:x 1, :y 0} {:x 2, :y 0}
            {:x 2, :y 1}
            {:x 1, :y 2} {:x 2, :y 2}]
           (->> example-1
                parse-asteroid-positions
                (set-center {:x 2 :y 2})))))

  (testing "adding angle and distance"
    (let [asteroids (->> example-1
                        parse-asteroid-positions
                        (set-center {:x 2 :y 2})
                        add-angle-and-distance)]
      (is (= [2.677945044588987 -2.356194490192345
              1.5707963267948966 1.5707963267948966 0.0 -1.5707963267948966 -1.5707963267948966
              -1.1071487177940904
              -0.4636476090008061 -0.7853981633974483]
             (map :angle asteroids)))

      (is (= [3 4
              2 1 0 1 2
              3
              3 4]
             (map :distance asteroids))))))


(deftest finding-nth-vaporized
  (testing "first round on first example"
    "
    .7..1
    .....
    .6#2.
    ....3
    ...54
    "
    (let [asteroids (->> example-1
                         parse-asteroid-positions
                         (add-relative-angle-and-distance {:x 2 :y 2})
                         sort-first-round-by-angle)]
      (is (= [{:x 4 :y 0} {:x 3 :y 2} {:x 4 :y 3} {:x 4 :y 4}
              {:x 3 :y 4} {:x 1 :y 2} {:x 1 :y 0}]
             (map #(select-keys % [:x :y]) asteroids)))))

  (def example-6 [".#....#####...#.."
                  "##...##.#####..##"
                  "##...#...#.#####."
                  "..#.....#...###.."
                  "..#.#.....#....##"])

  (testing "first round on sixth example"
    (let [asteroids (->> example-6
                         parse-asteroid-positions
                         (add-relative-angle-and-distance {:x 8 :y 3})
                         sort-first-round-by-angle
                         (map #(select-keys % [:x :y])))]
      (is (= [{:x 8 :y 1} {:x 9 :y 0} {:x 9 :y 1} {:x 10 :y 0}
              {:x 9 :y 2} {:x 11 :y 1} {:x 12 :y 1} {:x 11 :y 2}
              {:x 15 :y 1} {:x 12 :y 2} {:x 13 :y 2} {:x 14 :y 2}
              {:x 15 :y 2} {:x 12 :y 3} {:x 16 :y 4} {:x 15 :y 4}
              {:x 10 :y 4} {:x 4 :y 4} {:x 2 :y 4} {:x 2 :y 3}
              {:x 0 :y 2} {:x 1 :y 2} {:x 0 :y 1} {:x 1 :y 1}
              {:x 5 :y 2} {:x 1 :y 0} {:x 5 :y 1} {:x 6 :y 1}
              {:x 6 :y 0} {:x 7 :y 0}]
             asteroids))))

  (testing "first round on fifth example"
    (let [asteroids (->> example-5
                         parse-asteroid-positions
                         (add-relative-angle-and-distance {:x 11 :y 13})
                         sort-first-round-by-angle
                         (map #(select-keys % [:x :y])))]
      (is (= [{:x 11 :y 12} {:x 12 :y 1} {:x 12 :y 2}]
             (take 3 asteroids)))

      (is (= {:x 12 :y 8}
             (nth asteroids 9))))))
