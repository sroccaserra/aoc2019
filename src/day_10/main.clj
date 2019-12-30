(ns day-10.main
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-common-cli :refer [read-lines-from-stdin]]))

(defn asteroid-coordinates [lines]
  (apply concat
         (for [[row-number row] (map-indexed vector lines)]
           (keep-indexed #(when (= \# %2) {:x %1 :y row-number}) row))))

(defn -main [& args]
  (println (asteroid-coordinates (read-lines-from-stdin))))
