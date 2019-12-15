(ns advent-of-code-2019.day-02.main
  (:gen-class)
  (:require [clojure.string :as str]))

(defn read-ints-from-line [line]
   (map #(Integer/parseInt %) (str/split line #",")))

(defn read-lines-from-stdin []
  (line-seq (java.io.BufferedReader. *in*)))

(defn -main [& args]
  (println (-> (read-lines-from-stdin)
               first
               read-ints-from-line)))
