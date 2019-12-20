(ns day-03.main
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-common-cli :refer [read-lines-from-stdin]]
            [day-03.path :refer [compute-all-points find-closest-intersection]]
            [day-03.parser :refer [parse-commands]]))

(defn -main [& args]
  (let [[path-1 path-2] (->> (read-lines-from-stdin)
                             (map parse-commands)
                             (map compute-all-points))]
    (println (find-closest-intersection path-1 path-2))))
