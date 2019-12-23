(ns day-05.intcode)

(defn create-intcode-vm [program]
  {:memory program
   :pc 0})

(def ^:private operations {1 +
                           2 *})

(def ^:private halt-instruction {:opcode 99})

(defn- read-int-at [vm-state address]
  (get-in vm-state [:memory address]))

(defn- parameter-address [{pc :pc :as vm-state} n]
  {:pre [(<= n 3)]}
  (read-int-at vm-state (+ n pc)))

(defn- parameter-value [vm-state n]
  (->> (parameter-address vm-state n)
       (read-int-at vm-state)))

(defn- read-opcode [vm-state]
  (read-int-at vm-state (:pc vm-state)))

(defn- halted? [vm-state]
  (= (:opcode halt-instruction)
     (read-opcode vm-state)))

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

(defn step [vm-state]
  (-> vm-state
      read-instruction
      (execute-instruction vm-state)))


(defn run [vm-state]
  (if (halted?  vm-state)
    vm-state
    (recur (step vm-state))))

(defn restore-state [{pc :pc :as vm-state} address-1 address-2]
  (-> vm-state
      (assoc-in [:memory (+ 1 pc)] address-1)
      (assoc-in [:memory (+ 2 pc)] address-2)))
