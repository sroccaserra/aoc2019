(ns day-14.main
  (:gen-class)
  (:require [clojure.edn :as edn]))

(defn -main [& args]
  (-> "resources/day_14/input.edn"
      slurp
      edn/read-string
      prn))
