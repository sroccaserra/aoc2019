(ns day-05.intcode)

(def ^:private operations {1 +
                           2 *})

(def ^:private halt-instruction {:opcode 99})

(defn create-intcode-vm [program]
  {:memory program
   :pc 0})

(defn- read-int-at [vm-state address]
  (get-in vm-state [:memory address]))

(defn parameter-address [{pc :pc :as vm-state} n]
  {:pre [(<= n 3)]}
  (read-int-at vm-state (+ n pc)))

(defn- parameter-value [vm-state n]
  (->> (parameter-address vm-state n)
       (read-int-at vm-state)))

(defn- read-opcode [vm-state]
  (read-int-at vm-state (:pc vm-state)))

(defn read-instruction [vm-state]
  (let [opcode (read-opcode vm-state)]
    (if (= (:opcode halt-instruction) opcode)
      halt-instruction
      (let [operation (get operations opcode)
            value-1 (parameter-value vm-state 1)
            value-2 (parameter-value vm-state 2)]
        {:opcode opcode
         :result (operation value-1 value-2)
         :dest (parameter-address vm-state 3)
         :size 4}))))

(defn halted? [vm-state]
  (= halt-instruction (read-instruction vm-state)))

(defn step [vm-state]
  (let [instruction (read-instruction vm-state)]
    (-> vm-state
        (assoc-in [:memory (:dest instruction)] (:result instruction))
        (update-in [:pc] + (:size instruction)))))

(defn run [vm-state]
  (if (halted?  vm-state)
    vm-state
    (recur (step vm-state))))

(defn restore-state [{pc :pc :as vm-state} address-1 address-2]
  (-> vm-state
      (assoc-in [:memory (+ 1 pc)] address-1)
      (assoc-in [:memory (+ 2 pc)] address-2)))
