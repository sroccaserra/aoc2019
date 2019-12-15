(ns advent-of-code-2019.day-01.main
  (:gen-class))

(defn parse-lines-as-integers [lines]
  (map #(Integer/parseInt %) lines))

(defn read-lines-from-stdin []
  (line-seq (java.io.BufferedReader. *in*)))

(defn -main [& args]
  (doseq [line (parse-lines-as-integers (read-lines-from-stdin))]
    (println line)))
