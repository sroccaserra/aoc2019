(ns advent-of-code-2019.day-02.main
  (:gen-class))

(defn read-lines-from-stdin []
  (line-seq (java.io.BufferedReader. *in*)))

(defn -main [& args]
  (println (read-lines-from-stdin)))

