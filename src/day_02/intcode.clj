(ns day-02.intcode)

(defn create-intcode-vm [program]
  {:memory program
   :pc 0})

(defn value-at [vm-state address]
  (nth (:memory vm-state) address))

(defn opcode [vm-state]
  (value-at vm-state (:pc vm-state)))

(defn parameter-1-address [vm-state]
  (nth (:memory vm-state) (+ 1 (:pc vm-state))))

(defn parameter-2-address [vm-state]
  (nth (:memory vm-state) (+ 2 (:pc vm-state))))

(defn parameter-3-address [vm-state]
  (nth (:memory vm-state) (+ 3 (:pc vm-state))))

(defn- parameter-1-value [vm-state]
  (->> vm-state
       parameter-1-address
       (value-at vm-state)))

(defn- parameter-2-value [vm-state]
  (->> vm-state
       parameter-2-address
       (value-at vm-state)))

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

(defn run [vm]
  (loop [vm-state vm]
    (if (= 99 (opcode vm-state))
      vm-state
      (recur (step vm-state)))))

(defn restore-state [vm address-1 address-2]
  (assoc vm
         :memory (assoc (:memory vm)
                     (+ 1 (:pc vm)) address-1
                     (+ 2 (:pc vm)) address-2)))
