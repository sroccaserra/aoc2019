(ns advent-of-code-2019.day-03.main
  (:gen-class)
  (:require [clojure.string :as str]
            [advent-of-code-2019.day-03.path
             :refer [parse-commands compute-path]]))

(defn read-lines-from-stdin []
  (line-seq (java.io.BufferedReader. *in*)))

(defn -main [& args]
  (println (->> (read-lines-from-stdin)
                (map parse-commands)
                )))
