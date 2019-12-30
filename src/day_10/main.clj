(ns day-10.main
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-common-cli :refer [read-lines-from-stdin]]))

(defn parse-asteroid-positions [lines]
  (apply concat
         (for [[row-number row] (map-indexed vector lines)]
           (keep-indexed #(when (= \# %2) {:x %1 :y row-number}) row))))

(defn slope [point-1 point-2]
  (let [dx (apply - (map :x [point-2 point-1]))
        dy (apply - (map :y [point-2 point-1]))]
    (cond
      (and (zero? dx) (zero? dy)) nil
      (zero? dx) (if (pos? dy)
                   :infinite
                   :-infinite)
      (zero? dy) (if (pos? dx)
                   :zero
                   :-zero)
      :else (/ dy dx))))

(defn count-unique-slopes [asteroid asteroids]
  (->> (map #(slope asteroid %) asteroids)
       (filter (comp not nil?))
       set
       count))

(defn count-detected-from-best-location [asteroids]
  (->> asteroids
       (map #(count-unique-slopes % asteroids))
       (apply max)))

(defn -main [& args]
  (let [asteroids (parse-asteroid-positions (read-lines-from-stdin))]
    (println (count-detected-from-best-location asteroids))))
