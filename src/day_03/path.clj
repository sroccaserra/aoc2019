(ns day-03.path
  (:require [clojure.set :as set]))

(defn point [x y]
  {:x x :y y})

(def origin (point 0 0))
(def empty-path [origin])

(def ^{:private true}
  increments-for-direction {'U [0 1]
                            'D [0 -1]
                            'L [-1 0]
                            'R [1 0]})

(defn add-command-points [path command]
  (let [[direction nb-steps] command
        [increment-x increment-y] (direction increments-for-direction)
        steps (range 1 (inc nb-steps))
        {last-x :x last-y :y} (last path)
        xs (map #(+ last-x (* increment-x %)) steps)
        ys (map #(+ last-y (* increment-y %)) steps)]
    (into [] (concat path (map point xs ys)))))

(defn compute-all-points [commands]
  (reduce add-command-points empty-path commands))

(defn path-intersections [path-1 path-2]
  (disj (set/intersection (set path-1) (set path-2))
        origin))

(defn manhattan-distance [a-point]
  (+ (Math/abs (:x a-point)) (Math/abs (:y a-point))))

(defn find-closest-intersection [path-1 path-2]
  (->> (path-intersections path-1 path-2)
       (map manhattan-distance)
       (apply min)))

;; Part 2

(defn find-shortest-wire-intersection [path-1 path-2]
  (->> (path-intersections path-1 path-2)
       (map #(+ (.indexOf path-1 %) (.indexOf path-2 %)))
       (apply min)))
