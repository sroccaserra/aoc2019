(ns day-09.main
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-common-cli :refer [read-lines-from-stdin]]
            [day-09.intcode-vm-state :refer [create-intcode-vm]]
            [day-09.intcode-run :refer [run]]))

(defn read-ints-from-line [line]
  (->> (str/split line #",")
       (mapv #(Integer/parseInt %))))

(defn -main [& args]
  (let [program (-> (read-lines-from-stdin)
                    first
                    read-ints-from-line)]
    (-> (create-intcode-vm program :inputs [1] :memory-size 1028)
        run
        :outputs
        println)
    (-> (create-intcode-vm program :inputs [2] :memory-size 1076)
        run
        :outputs
        println)))
