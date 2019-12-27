(ns day-07.intcode-vm-state)

(def halt-opcode 99)

(def position-mode 0)
(def immediate-mode 1)

(defn create-intcode-vm [program & [input]]
  {:memory program
   :pc 0
   :input input
   :output []})

(defn- read-int-at [vm-state address]
  (get-in vm-state [:memory address]))

(defn write-int-at [vm-state address value]
  (assoc-in vm-state [:memory address] value))

(defn add-output-value [{output :output :as vm-state} output-value]
  (assoc vm-state :output (conj output output-value)))

(defn parameter-value [{pc :pc :as vm-state} n mode]
  {:pre [(<= n 3)]}
  (let [value (read-int-at vm-state (+ n pc))]
  (condp = mode
    immediate-mode value
    position-mode (read-int-at vm-state value)
    :else (throw (AssertionError. "Unsupported mode.")))))

(defn increment-pc [vm-state n]
  (update-in vm-state [:pc] + n))

(defn set-pc [vm-state n]
  (assoc vm-state :pc n))

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
