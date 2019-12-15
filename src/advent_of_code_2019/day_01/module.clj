(ns advent-of-code-2019.day-01.module)

(defn fuel-for-mass [mass]
  (-> mass
      (quot 3)
      (- 2)))

(defn fuel-for-module-mass [module-mass]
  (loop [sum 0
         remaining-mass module-mass]
    (let [new-required-fuel (fuel-for-mass remaining-mass)]
      (if (<= new-required-fuel 0)
        sum
        (recur (+ sum new-required-fuel)
               new-required-fuel)))))

(defn compute-required-fuel-for-module-masses [module-masses]
  (->> module-masses
       (map fuel-for-module-mass)
       (apply +)))
