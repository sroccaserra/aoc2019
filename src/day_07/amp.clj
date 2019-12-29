(ns day-07.amp
  (:require [intcode.vm-state :refer [create-intcode-vm add-inputs drop-outputs needs-input? read-input halted?]]
            [intcode.run :refer [run step]]))

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
                     (create-intcode-vm :inputs [phase input-signal])
                     run
                     :outputs first)]
      (recur program (rest phase-settings) output))))

(defn run-until-need-input [vm-state]
  (cond
    (halted? vm-state) vm-state
    (needs-input? vm-state) vm-state
    :else (recur (step vm-state))))

(defn run-amp-loop
  ([program phase-settings input-signal]
   (let [vm-seq (mapv #(create-intcode-vm program :inputs [%]) phase-settings)]
     (run-amp-loop (assoc vm-seq 0
                          (-> vm-seq first (add-inputs [input-signal]))))))
  ([{first-vm 0 second-vm 1 :as vm-seq}]
   (if (halted? first-vm)
     (read-input first-vm)
     (let [running-vm (run-until-need-input first-vm)
           results (:outputs running-vm)
           first-vm' (drop-outputs running-vm)
           second-vm' (add-inputs second-vm results)]
       (recur [second-vm' (get vm-seq 2) (get vm-seq 3) (get vm-seq 4) first-vm'])))))
