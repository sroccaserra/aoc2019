(ns day-02.main
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
    (println (for [noun (range 99)
                   verb (range 99)
                   :let [result (-> program
                                    (assoc 1 noun 2 verb)
                                    create-intcode-vm
                                    run
                                    (get-in [:memory 0]))]
                   :when (= 19690720 result)]
               {:result result
                :noun noun
                :verb verb}))))
