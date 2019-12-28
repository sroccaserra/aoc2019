(ns day-07.intcode-vm-state)

(def halt-opcode 99)

(def position-mode 0)
(def immediate-mode 1)

(defn create-intcode-vm [program & {:keys [inputs]
                                    :or {inputs []}}]
  {:memory program
   :pc 0
   :inputs (into clojure.lang.PersistentQueue/EMPTY inputs)
   :outputs []})

;; Memory

(defn- read-int-at [vm-state address]
  (get-in vm-state [:memory address]))

(defn write-int-at [vm-state address value]
  (assoc-in vm-state [:memory address] value))

;; Program Counter

(defn increment-pc [vm-state n]
  (update-in vm-state [:pc] + n))

(defn set-pc [vm-state n]
  (assoc vm-state :pc n))

;; Opcode and parameter modes

(defn read-first-instruction-value [vm-state]
  (read-int-at vm-state (:pc vm-state)))

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

(defn- read-opcode [vm-state]
  (-> vm-state
      read-first-instruction-value
      parse-first-instruction-value
      :opcode))

(defn halted? [vm-state]
  (= halt-opcode
     (read-opcode vm-state)))

;; Parameters

(defn parameter-value [{pc :pc :as vm-state} n mode]
  {:pre [(<= n 3)]}
  (let [value (read-int-at vm-state (+ n pc))]
  (condp = mode
    immediate-mode value
    position-mode (read-int-at vm-state value)
    :else (throw (AssertionError. "Unsupported mode.")))))

;; Inputs and outputs

(defn read-input [vm-state]
  (peek (:inputs vm-state)))

(defn add-input [{inputs :inputs :as vm-state} input-value]
  (assoc vm-state :inputs (conj inputs input-value)))

(defn drop-input [{inputs :inputs :as vm-state}]
  (assoc vm-state :inputs (pop inputs)))

(defn add-output [{outputs :outputs :as vm-state} output-value]
  (assoc vm-state :outputs (conj outputs output-value)))
