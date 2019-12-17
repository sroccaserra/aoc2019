(ns day-03.path
  (:require [clojure.set :as set]))

(def ^{:private true} increments {'U [0 1]
                                  'D [0 -1]
                                  'L [-1 0]
                                  'R [1 0]})

(def empty-path [[0 0]])

(defn- apply-move [increment path]
  (conj path (map + increment (last path))))

(defn eval-command [path command]
  (let [[direction nb-steps] command
        base-increment (direction increments)
        increment (map (partial * nb-steps) base-increment)]
    (apply-move increment path)))

(defn compute-path [commands]
  (reduce eval-command empty-path commands))

(defn compute-path-segments [path]
  (for [point-1 path
        point-2 (rest path)]
    [point-1 point-2]))

(defn all-points-between [segment]
  (let [[[x-1 y-1] [x-2 y-2]] segment
        x-min (min x-1 x-2)
        x-max (max x-1 x-2)
        y-min (min y-1 y-2)
        y-max (max y-1 y-2)]
    (set (for [x (range x-min (inc x-max))
               y (range y-min (inc y-max))]
           [x y]))))

(defn segment-intersection [segment-1 segment-2]
  (set/intersection (all-points-between segment-1)
                    (all-points-between segment-2)))
