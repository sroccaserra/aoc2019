(ns day-07.amp
  (:require [day-07.intcode-vm-state :refer [create-intcode-vm]]
            [day-07.intcode-run :refer [run]]))

(defn permutations [colls]
  (if (= 1 (count colls))
    (list colls)
    (for [head colls
          tail (permutations (disj (set colls) head))]
      (cons head tail))))

(defn run-amp-chain [program phase-settings input-signal]
  (if (empty? phase-settings)
    input-signal
    (let [phase (first phase-settings)
          output (-> program
                     (create-intcode-vm :input [phase input-signal])
                     run
                     :output first)]
      (recur program (rest phase-settings) output))))
