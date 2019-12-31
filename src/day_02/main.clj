(ns day-02.main
  (:gen-class)
  (:require [aoc-common-cli :refer [read-intcode-program-from-stdin]]
            [intcode.vm-state :refer [create-intcode-vm]]
            [intcode.run :refer [run]]))

(defn -main [& args]
  (let [program (read-intcode-program-from-stdin)]
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
