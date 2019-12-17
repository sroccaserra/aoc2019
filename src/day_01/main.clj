(ns day-01.main
  (:gen-class)
  (:require [aoc-common-cli :refer [read-lines-from-stdin]]
            [day-01.module
             :refer [compute-required-fuel-for-module-masses]]))

(defn parse-lines-as-integers [lines]
  (map #(Integer/parseInt %) lines))

(defn -main [& args]
  (println (->> (read-lines-from-stdin)
                parse-lines-as-integers
                compute-required-fuel-for-module-masses)))
