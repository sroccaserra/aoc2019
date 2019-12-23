(ns day-05.instruction
  (:require [day-05.vm-state :refer :all]))

(def ^:private operations {1 +
                           2 *})

(def ^:private halt-instruction {:opcode 99})

(defn create-add-instruction [vm-state]
  {:opcode 1
   :parameter-1 (parameter-value vm-state 1)
   :parameter-2 (parameter-value vm-state 2)
   :dest (parameter-address vm-state 3)
   :size 4})

(defn create-mul-instruction [vm-state]
  {:opcode 2
   :parameter-1 (parameter-value vm-state 1)
   :parameter-2 (parameter-value vm-state 2)
   :dest (parameter-address vm-state 3)
   :size 4})

(defn read-instruction [vm-state]
  (let [opcode (read-opcode vm-state)]
    (condp = opcode
      (:opcode halt-instruction) halt-instruction
      1 (create-add-instruction vm-state)
      2 (create-mul-instruction vm-state))))

(defn execute-instruction [{:keys [opcode size parameter-1 parameter-2 dest]} vm-state]
  (let [operation (get operations opcode)
        result (operation parameter-1 parameter-2)]
    (-> vm-state
        (assoc-in [:memory dest] result)
        (update-in [:pc] + size))))
