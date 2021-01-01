(ns day-17.main
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-common-cli :refer [read-intcode-program-from-file]]
            [intcode.vm-state :refer [create-intcode-vm add-input drop-outputs]]
            [intcode.run :refer [run run-until-needs-input]]))

(defn char-at [[x y] lines]
  (-> lines (nth y) (nth x)))

(defn is-in-bound? [[x y] w h]
  (and (<= 0 x) (< x w) (<= 0 y) (< y h)))

(defn neighbors [[x y] lines]
  (let [w (count (first lines))
        h (count lines)
        coords [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]]
    (->> coords
         (filter #(is-in-bound? % w h))
         (map #(char-at % lines)))))

(defn is-intersection? [[x y] lines]
  (and
    (= (char-at [x y] lines) \#)
    (= (neighbors [x y] lines) [\# \# \# \#])))

(defn find-intersections [lines]
  (let [w (count (first lines))
        h (count lines)]
    (for [y (range h)
          x (range w)
          :when (is-intersection? [x y] lines)]
      [x y])))

(defn display-scaffolding [lines]
  (->> lines (str/join "\n") println))

(defn as-lines [outputs]
  (->> outputs (map char) (apply str) str/split-lines))

(defn -main [& args]
  (let [program (read-intcode-program-from-file "resources/day_17/input.txt")
        vm (create-intcode-vm program :memory-size 4000)
        lines (as-lines (:outputs (run vm)))]
    (display-scaffolding lines)
    (prn (reduce (fn [a [x y]] (+ a (* x y))) 0 (find-intersections lines)))))
