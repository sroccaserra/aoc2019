(ns day-10.main
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-common-cli :refer [read-lines-from-stdin]]))

(defn parse-asteroid-positions [lines]
  (apply concat
         (for [[row-number row] (map-indexed vector lines)]
           (keep-indexed #(when (= \# %2) {:x %1 :y row-number}) row))))

(defn slope-with-quadrant-info [point-1 point-2]
  (let [dx (- (:x point-2) (:x point-1))
        dy (- (:y point-1) (:y point-2))]
    (cond
      (and (zero? dx) (zero? dy)) nil
      (zero? dx) [:inf (pos? dx) (pos? dy)]
      :else [(/ dy dx) (pos? dx) (pos? dy)])))

(defn slopes-for-asteroid [asteroid asteroids]
  (map #(slope-with-quadrant-info asteroid %) asteroids))

(defn count-unique-slopes [asteroid asteroids]
  (->> asteroids
       (slopes-for-asteroid asteroid)
       (filter (comp not nil?))
       set
       count))

(defn find-best-location [asteroids]
  (->> asteroids
       (map #(merge % {:nb-seen (count-unique-slopes % asteroids)}))
       (apply max-key :nb-seen)))

(defn -main [& args]
  (let [asteroids (parse-asteroid-positions (read-lines-from-stdin))
        best-location (find-best-location asteroids)]
    (println best-location)))
