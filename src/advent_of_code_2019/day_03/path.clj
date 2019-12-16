(ns advent-of-code-2019.day-03.path
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def ^{:private true} moves {'U [identity inc]
                             'D [identity dec]
                             'L [dec identity]
                             'R [inc identity]})

(def start-position '([0 0]))

(defn parse-command [command-string]
  (->> [(subs command-string 0 1) (subs command-string 1)]
    (map edn/read-string)))

(defn parse-commands [commands-string]
  (->> (str/split commands-string #",")
       (map parse-command)))

(defn- apply-move [move path]
  (let [position (first path)
        new-position (map #(%1 %2) move position)]
    (conj path new-position)))

(defn eval-command [command path]
  (let [[direction nb-steps] command
        [[x y]] path
        move (direction moves)]
    (nth (iterate (partial apply-move move) path)
         nb-steps)))

(defn compute-path
  ([commands] (compute-path commands start-position))
  ([commands path]
   (if (empty? commands)
     path
     (recur (rest commands)
            (eval-command (first commands) path)))))
