(ns day-05.intcode)

(defn create-intcode-vm [program]
  {:memory program
   :pc 0})

(defn read-value-at [vm-state address]
  (nth (:memory vm-state) address))

(defn read-instruction [vm-state]
  {:opcode (read-value-at vm-state (:pc vm-state))})

(defn opcode [vm-state]
  (:opcode (read-instruction vm-state)))

(defn halted? [vm-state]
  (= 99 (:opcode (read-instruction vm-state))))

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

(def operations {1 +
                 2 *})

(defn step [vm-state]
  (let [operation (get operations (opcode vm-state))
        values (instruction-values vm-state)]
    (assoc vm-state
           :memory (assoc (:memory vm-state)
                       (parameter-3-address vm-state)
                       (apply operation values))
           :pc (+ 4 (:pc vm-state)))))

(defn run [vm-state]
  (if (halted?  vm-state)
    vm-state
    (recur (step vm-state))))

(defn restore-state [vm address-1 address-2]
  (assoc vm
         :memory (assoc (:memory vm)
                     (+ 1 (:pc vm)) address-1
                     (+ 2 (:pc vm)) address-2)))
