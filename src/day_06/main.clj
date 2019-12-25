(ns day-06.main
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [aoc-common-cli :refer [read-lines-from-stdin]]))

(defn -main [& args]
  (-> (read-lines-from-stdin)
      str/join
      edn/read-string
      println))
