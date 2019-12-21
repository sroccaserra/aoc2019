(ns day-05.intcode)

(def ^:private operations {1 +
                           2 *})

(def ^:private halt-instruction {:opcode 99})

(defn create-intcode-vm [program]
  {:memory program
   :pc 0})

(defn read-value-at [vm-state address]
  (nth (:memory vm-state) address))

(defn parameter-1-address [vm-state]
  (nth (:memory vm-state) (+ 1 (:pc vm-state))))

(defn parameter-2-address [vm-state]
  (nth (:memory vm-state) (+ 2 (:pc vm-state))))

(defn parameter-3-address [vm-state]
  (nth (:memory vm-state) (+ 3 (:pc vm-state))))

(defn- parameter-1-value [vm-state]
  (->> vm-state
       parameter-1-address
       (read-value-at vm-state)))

(defn- parameter-2-value [vm-state]
  (->> vm-state
       parameter-2-address
       (read-value-at vm-state)))

(defn instruction-values [vm-state]
  [(parameter-1-value vm-state)
   (parameter-2-value vm-state)])

(defn- read-opcode [vm-state]
  (read-value-at vm-state (:pc vm-state)))

(defn read-instruction [vm-state]
  (let [opcode (read-opcode vm-state)]
    (if (= (:opcode halt-instruction) opcode)
      halt-instruction
      (let [operation (get operations opcode)
            values (instruction-values vm-state)]
        {:opcode opcode
         :result (apply operation values)
         :dest (parameter-3-address vm-state)
         :size 4}))))

(defn halted? [vm-state]
  (= halt-instruction (read-instruction vm-state)))

(defn step [vm-state]
  (let [instruction (read-instruction vm-state) ]
    (assoc vm-state
           :memory (assoc (:memory vm-state)
                          (:dest instruction)
                          (:result instruction))
           :pc (+ (:size instruction) (:pc vm-state)))))

(defn run [vm-state]
  (if (halted?  vm-state)
    vm-state
    (recur (step vm-state))))

(defn restore-state [vm address-1 address-2]
  (assoc vm
         :memory (assoc (:memory vm)
                        (+ 1 (:pc vm)) address-1
                        (+ 2 (:pc vm)) address-2)))
