(ns day-23.main
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-common-cli :refer [read-intcode-program-from-file]]
            [intcode.vm-state :refer [create-intcode-vm add-inputs write-int-at]]
            [intcode.run :refer [run run-until-needs-input]]))

(def nb-vms 50)

(defn dispatch-packet [vms [address x y]]
  (let [vm (nth vms address)]
    (assoc vms address (add-inputs vm [x y]))))

(defn dispatch-packets [packets vms]
  (reduce dispatch-packet vms packets))

(defn list-packets [vms]
  (partition 3 (mapcat :outputs vms)))

(defn step [vms]
  (let [vms' (vec (map run-until-needs-input vms))
        packets (list-packets vms')]
    (dispatch-packets packets vms')))

(defn -main [& args]
  (let [intcode-program (read-intcode-program-from-file "resources/day_23/input.txt")
        vms (vec (map #(create-intcode-vm intcode-program :inputs [% -1]) (range nb-vms)))]
          (run! prn (list-packets (step vms)))))
