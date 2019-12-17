(ns day-01.main
  (:gen-class)
  (:require [day-01.module
             :refer [compute-required-fuel-for-module-masses]]))

(defn parse-lines-as-integers [lines]
  (map #(Integer/parseInt %) lines))

(defn read-lines-from-stdin []
  (line-seq (java.io.BufferedReader. *in*)))

(defn -main [& args]
  (println (->> (read-lines-from-stdin)
                parse-lines-as-integers
                compute-required-fuel-for-module-masses)))
