(ns day-10.main
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-common-cli :refer [read-lines-from-stdin]]))

(defn -main [& args]
  (doseq [line (read-lines-from-stdin)]
    (println line)))
