(ns day-13.main
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-common-cli :refer [read-intcode-program-from-stdin]]
            [intcode.vm-state :refer [create-intcode-vm add-input]]
            [intcode.run :refer [run run-until-needs-input]]))

(def tiles {0 \.
            1 \█
            2 \▒
            3 \=
            4 \o})

(def w 42)
(def h 24)

(def empty-screen (into [] (repeat (* w h) \?)))

(defn screen-to-str [screen]
  (->> screen
       (partition w)
       (map #(apply str %))
       (str/join "\n")))

(defn draw-tile-to-screen [screen [x y tile-id]]
  (assoc screen (+ (* y w) x) (tiles tile-id)))

(defn draw-tiles-to-screen [screen tiles]
  (if (empty? tiles)
    screen
    (recur (draw-tile-to-screen screen (first tiles))
           (rest tiles))))

(defn -main [& args]
  (let [program (read-intcode-program-from-stdin)
        vm (create-intcode-vm program :memory-size 2659)]
    (prn (->> (run vm)
              :outputs
              (drop 2)
              (take-nth 3)
              (filter #(= 2 %))
              count))
    (println (->> (add-input vm 2)
                  run-until-needs-input
                  :outputs
                  (partition 3)
                  (draw-tiles-to-screen empty-screen)
                  screen-to-str))))
