(ns day-23.main
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-common-cli :refer [read-intcode-program-from-file]]
            [intcode.vm-state :refer [create-intcode-vm add-inputs drop-outputs]]
            [intcode.run :refer [run-until-needs-input]]))

(def nb-vms 50)

(defn dispatch-packet [vms [address x y]]
  (let [vm (nth vms address)]
    (assoc vms address (add-inputs vm [x y]))))

(defn dispatch-nat-to-zero [vms [_ x y]]
  (dispatch-packet vms [0 x y]))

(defn dispatch-packets [packets vms]
  (reduce dispatch-packet vms packets))

(defn list-packets [vms]
  (partition 3 (mapcat :outputs vms)))

(defn find-first-255-packet [packets]
  (first (filter #(= 255 (first %)) packets)))

(defn find-nat-packet [packets]
  (last (filter #(= 255 (first %)) packets)))

(defn run-1 [vms]
  (let [vms' (map run-until-needs-input vms)
        vms'' (vec (map drop-outputs vms'))
        packets (list-packets vms')
        nat (find-first-255-packet packets)]
    (if nat
      (last nat)
      (recur (dispatch-packets packets vms'')))))

(defn run-2 [vms nat zero-ys]
  (let [idle? (and (empty? (mapcat :inputs vms))
                   (empty? (mapcat :outputs vms)))]
    (cond
      (and idle? (= (last zero-ys) (last nat))) (last nat)
      idle? (recur (dispatch-nat-to-zero vms nat)
                   nat
                   (conj zero-ys (last nat)))
      :else (let [vms' (map run-until-needs-input vms)
                  vms'' (vec (map drop-outputs vms'))
                  packets (list-packets vms')
                  nat' (find-nat-packet packets)]
              (recur (dispatch-packets (filter #(not= 255 (first %)) packets) vms'')
                     (or nat' nat)
                     zero-ys)))))

(defn -main [& args]
  (let [intcode-program (read-intcode-program-from-file "resources/day_23/input.txt")
        vms (vec (map #(create-intcode-vm intcode-program :inputs [% -1] :memory-size 4000) (range nb-vms)))]
          (prn (run-1 vms))
          (prn (run-2 vms nil []))))
