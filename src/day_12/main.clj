(ns day-12.main
  (:gen-class))

(defn create-moon [x y z]
  {:position [x y z]
   :velocity [0 0 0]})

(def moons [(create-moon -13 -13 -13)
            (create-moon 5 -8 3)
            (create-moon -6 -10 -3)
            (create-moon  0 5 -5)])

;; math

(defn combinations [m n]
  (letfn [(comb-aux [m start]
            (if (= 1 m)
              (for [x (range start n)]
                (list x))
              (for [x (range start n)
                    xs (comb-aux (dec m) (inc x))]
                (cons x xs))))]
    (comb-aux m 0)))

;; moons gravity

(defn increment [value-1 value-2]
  (cond
    (< value-1 value-2) 1
    (> value-1 value-2) -1
    :else 0))

(defn update-velocity [moon other-moon-position]
  (let [increments (mapv increment (:position moon) other-moon-position)
        new-velocity (mapv + (:velocity moon) increments)]
    (assoc moon :velocity (mapv + (:velocity moon) increments))))

(defn apply-gravity-to-pair [index-1 index-2 moons]
  (let [moon-1 (nth moons index-1)
        moon-2 (nth moons index-2)]
    (assoc (into [] moons)
           index-1 (update-velocity moon-1 (:position moon-2))
           index-2 (update-velocity moon-2 (:position moon-1)))))

(defn combine-gravity [pairs moons]
  (if (empty? pairs)
    moons
    (let [[index-1 index-2] (first pairs)]
      (recur (rest pairs) (apply-gravity-to-pair index-1 index-2 moons)))))

(defn apply-gravity [moons]
  (combine-gravity (combinations 2 (count moons))
                   moons))

;; moons velocity

(defn apply-velocity [moons]
  (map #(assoc % :position (mapv + (:position %) (:velocity %)))
       moons))

;; system update

(defn step [moons]
  (-> moons
      apply-gravity
      apply-velocity))

;; system energy

(defn moon-energy [moon]
  (* (->> (:position moon)
          (map #(Math/abs %))
          (apply +))
     (->> (:velocity moon)
          (map #(Math/abs %))
          (apply +))))

(defn system-energy [moons]
  (apply + (map moon-energy moons)))

;; part 2

(defn slice-axis [axis moons]
  (for [{:keys [position velocity]} moons]
    {:position [(nth position axis)]
     :velocity [(nth velocity axis)]}))

(defn find-axis-cycle
  ([axis moons]
   (let [slice (slice-axis axis moons)]
     (find-axis-cycle slice 1 (step slice))))
  ([first-state n slice]
   (if (zero? (mod n 10000)) (println n "..."))
   (if (= first-state slice)
     (doto n prn)
     (recur first-state (inc n) (step slice)))))

(defn gcd
      [a b]
      (if (zero? b)
      a
      (recur b, (mod a b))))

(defn lcm-two
      [a b]
      (/ (* a b) (gcd a b)))

(defn lcm [& v] (reduce lcm-two v))

(defn find-repeating-state [moons]
  (let [x-cycle (find-axis-cycle 0 moons)
        y-cycle (find-axis-cycle 1 moons)
        z-cycle (find-axis-cycle 2 moons)]
    (lcm x-cycle y-cycle z-cycle)))

;; main

(defn -main [& args]
  (time (-> (iterate step moons)
            (nth 1000)
            (doto prn)
            system-energy
            prn))
  (prn (find-repeating-state moons)))
