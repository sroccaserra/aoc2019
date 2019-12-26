(ns day-06.main
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [aoc-common-cli :refer [read-lines-from-stdin]]))

(defn count-ancestors
  ([parent-map orbiter]
   (count-ancestors parent-map orbiter 0))
  ([parent-map orbiter n]
   (let [ancestor (parent-map orbiter)]
     (if (nil? ancestor)
       n
       (recur parent-map ancestor (inc n))))))

(defn -main [& args]
  (let [parent-map (-> (read-lines-from-stdin)
      str/join
      edn/read-string)
        orbiters (keys parent-map)]
    (println (reduce + (map #(count-ancestors parent-map %) orbiters)))))
