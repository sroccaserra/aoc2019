(ns advent-of-code-2019.day-02.main
  (:gen-class)
  (:require [clojure.string :as str]
            [advent-of-code-2019.day-02.intcode
             :refer [create-intcode-vm restore-state run]]))

(defn read-ints-from-line [line]
  (->> (str/split line #",")
       (map #(Integer/parseInt %))
       vec))

(defn read-lines-from-stdin []
  (line-seq (java.io.BufferedReader. *in*)))

(defn -main [& args]
  (let [program (-> (read-lines-from-stdin)
                    first
                    read-ints-from-line)
        vm (-> program
               create-intcode-vm
               (restore-state 12 2)
               run)]
        (println vm)))
