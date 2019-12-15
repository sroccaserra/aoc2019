(ns advent-of-code-2019.day-02.intcode)

(defn create-intcode-vm [program]
  {:ram program
   :pc 0})

(defn value-at [vm address]
  (nth (:ram vm) address))

(defn opcode [vm]
  (value-at vm (:pc vm)))

(defn position-src-1 [vm]
  (nth (:ram vm) (+ 1 (:pc vm))))

(defn position-src-2 [vm]
  (nth (:ram vm) (+ 2 (:pc vm))))

(defn position-dst [vm]
  (nth (:ram vm) (+ 3 (:pc vm))))

(defn value-1 [vm]
  (->> vm
       position-src-1
       (value-at vm)))

(defn value-2 [vm]
  (->> vm
       position-src-2
       (value-at vm)))

(defn arguments [vm]
  [(value-1 vm) (value-2 vm)])

(def operations {1 +
                 2 *})

(defn step [vm]
  (let [ram (:ram vm)
        pc (:pc vm)
        operation (get operations (opcode vm))
        dst-address (position-dst vm)]
    (assoc vm
           :ram (assoc ram
                       dst-address
                       (apply operation (arguments vm)))
           :pc (+ 4 pc))))

(defn run [vm]
  (loop [current-step vm]
    (if (= 99 (opcode current-step))
      current-step
      (recur (step current-step)))))

(defn restore-state [vm position-1 position-2]
  (assoc vm
         :ram (assoc (:ram vm)
                     (+ 1 (:pc vm)) position-1
                     (+ 2 (:pc vm)) position-2)))
