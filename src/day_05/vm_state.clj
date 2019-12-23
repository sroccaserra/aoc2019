(ns day-05.vm-state)

(def halt-opcode 99)

(defn- read-int-at [vm-state address]
  (get-in vm-state [:memory address]))

(defn write-int-at [vm-state address value]
  (assoc-in vm-state [:memory address] value))

(defn parameter-address [{pc :pc :as vm-state} n]
  {:pre [(<= n 3)]}
  (read-int-at vm-state (+ n pc)))

(defn parameter-value [vm-state n]
  (->> (parameter-address vm-state n)
       (read-int-at vm-state)))

(defn increment-pc [vm-state n]
  (update-in vm-state [:pc] + 4))

(defn read-opcode [vm-state]
  (read-int-at vm-state (:pc vm-state)))

(defn halted? [vm-state]
  (= halt-opcode
     (read-opcode vm-state)))

