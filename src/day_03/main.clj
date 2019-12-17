(ns day-03.main
  (:gen-class)
  (:require [clojure.string :as str]
            [day-03.path :refer [parse-commands compute-path]]))

(defn read-lines-from-stdin []
  (line-seq (java.io.BufferedReader. *in*)))

(defn -main [& args]
  (println (->> (read-lines-from-stdin)
                (map parse-commands)
                (map compute-path))))
