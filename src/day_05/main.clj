(ns day-05.main
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-common-cli :refer [read-lines-from-stdin]]
            [intcode.vm-state :refer [create-intcode-vm]]
            [intcode.run :refer [run]]))

(defn read-ints-from-line [line]
  (->> (str/split line #",")
       (mapv #(Integer/parseInt %))))

(defn -main [& args]
  (let [program (-> (read-lines-from-stdin)
                    first
                    read-ints-from-line)]
    (-> program
        (create-intcode-vm :inputs [5])
        run
        println)))
