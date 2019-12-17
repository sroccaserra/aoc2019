(ns day-03.main
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-common-cli :refer [read-lines-from-stdin]]
            [day-03.path :refer [compute-path]]
            [day-03.parser :refer [parse-commands]]))

(defn -main [& args]
  (println (->> (read-lines-from-stdin)
                (map parse-commands)
                (map compute-path))))
