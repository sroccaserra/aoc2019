(ns advent-of-code-2019.day-03.path
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.core.match :refer [match]]))

(defn parse-command [command]
  (->> (str/split command #"")
    (map edn/read-string)))

(defn move-up [positions]
  (let [[[x y]] positions]
    (conj positions [x (inc y)])))

(defn move-right [positions]
  (let [[[x y]] positions]
    (conj positions [(inc x) y])))

(defn eval-command [command positions]
  (let [[direction steps] command
        [[x y]] positions]
    (match [direction]
           ['U] (move-up positions)
           ['R] (move-right positions))))
