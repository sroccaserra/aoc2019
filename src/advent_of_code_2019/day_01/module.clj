(ns advent-of-code-2019.day-01.module)

(defn fuel-for-module-mass [module-mass]
  (-> module-mass
      (quot 3)
      (- 2)))

(defn compute-required-fuel-for-module-masses [module-masses]
  (->> module-masses
      (map fuel-for-module-mass)
      (apply +)))
