(ns day-03.main
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-common-cli :refer [read-lines-from-stdin]]
            [day-03.path :refer [compute-path path-intersections manhattan-distance]]
            [day-03.parser :refer [parse-commands]]))

(defn -main [& args]
  (let [[path-1 path-2] (->> (read-lines-from-stdin)
                             (map parse-commands)
                             (map compute-path))]
    (println (->> (path-intersections path-1 path-2)
                  (map manhattan-distance)
                  (apply min)))))
