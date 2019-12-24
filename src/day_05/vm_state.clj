(ns day-05.vm-state)

(def halt-opcode 99)

(defn- read-int-at [vm-state address]
  (get-in vm-state [:memory address]))

(defn write-int-at [vm-state address value]
  (assoc-in vm-state [:memory address] value))

(defn add-output-value [{output :output :as vm-state} output-value]
  (assoc vm-state :output (conj output output-value)))

(defn parameter-value [{pc :pc :as vm-state} n]
  {:pre [(<= n 3)]}
  (read-int-at vm-state (+ n pc)))

(defn parameter-value-indirect [vm-state n]
  (->> n
       (parameter-value vm-state)
       (read-int-at vm-state)))

(defn increment-pc [vm-state n]
  (update-in vm-state [:pc] + n))

(defn- split-to-reverse-digits [n]
  (->> n
       (iterate #(quot % 10))
       (take-while pos?)
       (mapv #(mod % 10))))

(defn parse-first-instruction-value [first-value]
  (let [reverse-digits (split-to-reverse-digits first-value)]
    {:opcode (+ (get reverse-digits 0)
                (* 10 (get reverse-digits 1 0)))
     :parameter-modes [(get reverse-digits 2 0)
                       (get reverse-digits 3 0)
                       (get reverse-digits 4 0)]}))


(defn read-first-instruction-value [vm-state]
  (read-int-at vm-state (:pc vm-state)))

(defn- read-opcode [vm-state]
  (-> vm-state
      read-first-instruction-value
      parse-first-instruction-value
      :opcode))

(defn halted? [vm-state]
  (= halt-opcode
     (read-opcode vm-state)))

(defn read-input [vm-state]
  (:input vm-state))
