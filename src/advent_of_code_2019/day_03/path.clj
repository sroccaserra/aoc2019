(ns advent-of-code-2019.day-03.path
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn parse-command [command]
  (->> (str/split command #"")
    (map edn/read-string)))

(def moves {'U [identity inc]
            'D [identity dec]
            'L [dec identity]
            'R [inc identity]})

(defn apply-move [move positions]
  (let [[position] positions
        new-position (map #(%1 %2) move position)]
    (conj positions new-position)))

(defn eval-command [command positions]
  (let [[direction nb-steps] command
        [[x y]] positions
        move (direction moves)]
    (nth (iterate (partial apply-move move) positions)
         nb-steps)))
