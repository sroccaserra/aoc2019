(ns day-03.path
  (:require [clojure.set :as set]))

(def ^{:private true}
  increments-for-direction {'U [0 1]
                            'D [0 -1]
                            'L [-1 0]
                            'R [1 0]})

(def origin [0 0])

(def empty-path [origin])

(defn add-command-points [path command]
  (let [[direction nb-steps] command
        [increment-x increment-y] (direction increments-for-direction)
        steps (range 1 (inc nb-steps))
        [last-x last-y] (last path)
        xs (map #(+ last-x (* increment-x %)) steps)
        ys (map #(+ last-y (* increment-y %)) steps)]
    (into [] (concat path (map (fn [x y] [x y]) xs ys)))))

(defn compute-all-points [commands]
  (reduce add-command-points empty-path commands))

(defn path-intersections [path-1 path-2]
  (disj
    (set/intersection (set path-1) (set path-2))
    origin))

(defn manhattan-distance [point]
  (let [[x y] point]
    (+ (Math/abs x) (Math/abs y))))
