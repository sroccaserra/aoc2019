(ns day-03.parser
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn parse-command [command-string]
  (->> [(subs command-string 0 1) (subs command-string 1)]
       (map edn/read-string)))

(defn parse-commands [commands-string]
  (->> (str/split commands-string #",")
       (map parse-command)))
