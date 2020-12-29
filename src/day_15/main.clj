(ns day-15.main
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-common-cli :refer [read-intcode-program-from-stdin]]
            [intcode.vm-state :refer [create-intcode-vm add-input drop-outputs]]
            [intcode.run :refer [run run-until-needs-input]]))

(defn create-maze-state []
  {:visited {[0 0] \.}
   :robot [0 0]})

(defn update-maze-state [maze-state output]
  maze-state)

(defn maze-state-as-string [{:keys [:visited :robot]}]
  (let [coords (keys visited)
        xs (map first coords)
        ys (map second coords)
        [x-min y-min x-max y-max] [(apply min xs) (apply min ys) (apply max xs) (apply max ys)]
        tiles (for [x (range x-min (inc x-max))
                    y (range y-min (inc y-max))]
                (if (= [x y] robot)
                  \@
                  (visited [x y] \▒)))]
    (->> tiles
         (partition (inc (- x-max x-min)))
         (map #(apply str %))
         (str/join "\n"))))

(defn reset-terminal []
  (println  "\033c"))

(defn draw-maze [maze-state]
  (comment reset-terminal)
  (println (maze-state-as-string maze-state)))

(defn has-found-oxygen [maze-state]
  true)

(defn update-vm-state [vm-state maze-state]
  vm-state)

(defn explore-maze
  ([vm-state]
   (explore-maze vm-state (create-maze-state)))
  ([vm-state maze-state]
   (let [next-vm-state (run-until-needs-input vm-state)
         maze-state' (update-maze-state maze-state
                                        (:outputs next-vm-state))]
     (draw-maze maze-state')
     (comment Thread/sleep 25)
     (if (has-found-oxygen maze-state')
       next-vm-state
       (recur (update-vm-state next-vm-state maze-state') maze-state')))))

(defn -main [& args]
  (let [program (read-intcode-program-from-stdin)
        vm (create-intcode-vm program :memory-size 2659)]
    (explore-maze vm )))
