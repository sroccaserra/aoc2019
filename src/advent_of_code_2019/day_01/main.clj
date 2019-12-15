(ns advent-of-code-2019.day-01.main
  (:gen-class))

(defn read-lines-from-stdin []
  (line-seq (java.io.BufferedReader. *in*)))

(defn -main [& args]
  (doseq [line (read-lines-from-stdin)]
    (println line)))

(defn fuel-for-module [mass]
  (- (quot mass 3) 2))
