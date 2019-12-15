(ns advent-of-code-2019.day-01.main
  (:gen-class))

(defn -main [& args]
  (println args))

(defn fuel-for-module [mass]
  (- (quot mass 3) 2))
