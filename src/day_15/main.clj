(ns day-15.main
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-common-cli :refer [read-intcode-program-from-file]]
            [intcode.vm-state :refer [create-intcode-vm add-input drop-outputs]]
            [intcode.run :refer [run run-until-needs-input]]))

(defn create-maze-state []
  {:visited {[0 0] \.}
   :robot [0 0]})

(defn add-wall [{[x y] :robot visited :visited} input]
  (cond (= 1 input) {:robot [x y] :visited (assoc visited [x (dec y)] \#)}
        (= 2 input) {:robot [x y] :visited (assoc visited [x (inc y)] \#)}
        (= 3 input) {:robot [x y] :visited (assoc visited [(dec x) y] \#)}
        (= 4 input) {:robot [x y] :visited (assoc visited [(inc x) y] \#)}))

(defn advance-robot [{[x y] :robot visited :visited} input]
  (cond (= 1 input) {:robot [x (dec y)] :visited (assoc visited [x (dec y)] \.)}
        (= 2 input) {:robot [x (inc y)] :visited (assoc visited [x (inc y)] \.)}
        (= 3 input) {:robot [(dec x) y] :visited (assoc visited [(dec x) y] \.)}
        (= 4 input) {:robot [(inc x) y] :visited (assoc visited [(inc x) y] \.)}))

(defn add-oxygen [{[x y] :robot visited :visited} input]
  (cond (= 1 input) {:robot [x (dec y)] :visited (assoc visited [x (dec y)] \o)}
        (= 2 input) {:robot [x (inc y)] :visited (assoc visited [x (inc y)] \o)}
        (= 3 input) {:robot [(dec x) y] :visited (assoc visited [(dec x) y] \o)}
        (= 4 input) {:robot [(inc x) y] :visited (assoc visited [(inc x) y] \o)}))

(defn update-maze-state [{robot :robot :as maze-state} input output]
  (cond (= 0 output) (add-wall maze-state input)
        (= 1 output) (advance-robot maze-state input)
        (= 2 output) (add-oxygen maze-state input)))

(defn maze-state-as-string [{:keys [:visited :robot]}]
  (let [coords (keys visited)
        xs (map first coords)
        ys (map second coords)
        [x-min y-min x-max y-max] [(apply min xs) (apply min ys) (apply max xs) (apply max ys)]
        tiles (for [y (range y-min (inc y-max))
                    x (range x-min (inc x-max))]
                (if (= [x y] robot)
                  \@
                  (visited [x y] \ )))]
    (->> tiles
         (partition (inc (- x-max x-min)))
         (map #(apply str %))
         (str/join "\n"))))

(defn reset-terminal []
  (println  "\033c"))

(defn draw-maze [maze-state]
  (reset-terminal)
  (println (maze-state-as-string maze-state)))

(defn has-found-oxygen [maze-state]
  false)

(defn update-vm-state [vm-state maze-state]
  vm-state)

(defn explore-maze
  ([vm-state]
   (explore-maze vm-state (create-maze-state)))
  ([vm-state maze-state]
   (let [command (Long/parseLong (read-line))
         next-vm-state (run-until-needs-input (add-input vm-state command))
         result (last (:outputs next-vm-state))
         maze-state' (update-maze-state maze-state command result)]
     (draw-maze maze-state')
     (comment Thread/sleep 25)
     (if (has-found-oxygen maze-state')
       next-vm-state
       (recur (update-vm-state next-vm-state maze-state') maze-state')))))

(defn -main [& args]
  (let [program (read-intcode-program-from-file "resources/day_15/input.txt")
        vm (create-intcode-vm program :inputs [4] :memory-size 2659)]
    (println "Ready to explore maze.")
    (explore-maze vm)))
