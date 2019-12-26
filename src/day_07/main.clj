(ns day-07.main
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-common-cli :refer [read-lines-from-stdin]]
            [day-07.intcode-vm-state :refer [create-intcode-vm]]
            [day-07.intcode-run :refer [run]]))

(defn read-ints-from-line [line]
  (->> (str/split line #",")
       (mapv #(Integer/parseInt %))))

(defn -main [& args]
  (let [program (-> (read-lines-from-stdin)
                    first
                    read-ints-from-line)]
    (-> program
        (create-intcode-vm 5)
        run
        println)))
