(ns advent-of-code-2019.day-03.path
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn parse-command [command]
  (->> (str/split command #"")
    (map edn/read-string)))

(defn move-up [positions]
  (let [[[x y]] positions]
    (conj positions [x (inc y)])))

(defn move-right [positions]
  (let [[[x y]] positions]
    (conj positions [(inc x) y])))

(def commands {'U move-up
               'R move-right})

(defn eval-command [command positions]
  (let [[direction steps] command
        [[x y]] positions
        command (direction commands)]
    (nth (iterate command positions)
         steps)))
