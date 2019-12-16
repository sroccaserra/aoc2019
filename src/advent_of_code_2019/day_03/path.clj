(ns advent-of-code-2019.day-03.path
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def ^{:private true} increments {'U [0 1]
                                  'D [0 -1]
                                  'L [-1 0]
                                  'R [1 0]})

(def start-position '([0 0]))

(defn parse-command [command-string]
  (->> [(subs command-string 0 1) (subs command-string 1)]
       (map edn/read-string)))

(defn parse-commands [commands-string]
  (->> (str/split commands-string #",")
       (map parse-command)))

(defn- apply-move [increment path]
  (conj path (map + increment (first path))))

(defn eval-command [path command]
  (let [[direction nb-steps] command
        increment (direction increments)]
    (nth (iterate (partial apply-move increment) path)
         nb-steps)))

(defn compute-path [commands]
  (reduce eval-command start-position commands))
