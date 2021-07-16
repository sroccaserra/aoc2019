(ns day-23.main
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-common-cli :refer [read-intcode-program-from-file]]
            [intcode.vm-state :refer [create-intcode-vm add-inputs write-int-at]]
            [intcode.run :refer [run run-until-needs-input]]))

(def nb-vms 50)

(defn step [vms]
  (map run-until-needs-input vms))

(defn packets [vms]
  (partition 3 (mapcat :outputs vms)))

(defn -main [& args]
  (let [intcode-program (read-intcode-program-from-file "resources/day_23/input.txt")
        vms (map #(create-intcode-vm intcode-program :inputs [% -1]) (range nb-vms))]
          (run! prn (packets (step vms)))))
