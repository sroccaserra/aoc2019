(ns day-06.main
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [aoc-common-cli :refer [read-lines-from-stdin]]))

(defn count-ancestors
  ([parent-map orbiter]
   (count-ancestors parent-map orbiter 0))
  ([parent-map orbiter n]
   (let [ancestor (parent-map orbiter)]
     (if (nil? ancestor)
       n
       (recur parent-map ancestor (inc n))))))

(defn find-ancestors
  ([parent-map orbiter]
   (find-ancestors parent-map orbiter #{}))
  ([parent-map orbiter ancestor-set]
   (let [ancestor (parent-map orbiter)]
     (if (nil? ancestor)
       ancestor-set
       (recur parent-map ancestor (conj ancestor-set ancestor))))))

(defn count-orbital-transfers [parent-map orbiter-1 orbiter-2]
  (let [ancestors-1 (find-ancestors parent-map orbiter-1)
        ancestors-2 (find-ancestors parent-map orbiter-2)
        only-1 (set/difference ancestors-1 ancestors-2)
        only-2 (set/difference ancestors-2 ancestors-1)]
    (+ (count only-1) (count only-2))))

(defn -main [& args]
  (let [parent-map (-> (read-lines-from-stdin)
                       str/join
                       edn/read-string)
        orbiters (keys parent-map)]
    (prn (reduce + (map #(count-ancestors parent-map %) orbiters))
         (count-orbital-transfers parent-map "YOU" "SAN"))))
