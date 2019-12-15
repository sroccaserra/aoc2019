(ns advent-of-code-2019.day-01.module)

(defn fuel-for-mass [mass]
  (-> mass
      (quot 3)
      (- 2)))

(defn fuel-for-module-mass [module-mass]
  (fuel-for-mass module-mass))

(defn compute-required-fuel-for-module-masses [module-masses]
  (->> module-masses
      (map fuel-for-module-mass)
      (apply +)))
