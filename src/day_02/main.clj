(ns day-02.main
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-common-cli :refer [read-lines-from-stdin]]
            [day-02.intcode
             :refer [create-intcode-vm restore-state run value-at]]))

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
                                    create-intcode-vm
                                    (restore-state noun verb)
                                    run
                                    (value-at 0))]
                   :when (= 19690720 result)]
               {:result result
                :noun noun
                :verb verb}))))
