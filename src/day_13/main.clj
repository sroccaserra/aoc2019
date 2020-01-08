(ns day-13.main
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-common-cli :refer [read-intcode-program-from-stdin]]
            [intcode.vm-state :refer [create-intcode-vm add-input drop-outputs]]
            [intcode.run :refer [run run-until-needs-input]]))

(def tiles {0 \.
            1 \█
            2 \▒
            3 \=
            4 \o})

(def w 42)
(def h 24)

(def empty-vram (into [] (repeat (* w h) \?)))

(defn reset-terminal []
  (println  "\033c"))

(defn vram-to-str [vram]
  (->> vram
       (partition w)
       (map #(apply str %))
       (str/join "\n")))

(defn draw-tile-to-vram [vram [x y tile-id]]
  (assoc vram (+ (* y w) x) (tiles tile-id)))

(defn has-block-tiles [vram]
  (some #(= (tiles 2) %) vram))

(defn is-score-tile [tile]
  (= [-1 0] (take 2 tile)))

(defn find-paddle-tile [tiles]
  (some #(when (= 3 (nth % 2)) %) tiles))

(defn find-ball-tile [tiles]
  (some #(when (= 4 (nth % 2)) %) tiles))

(defn find-score-tile [tiles]
  (some #(when (is-score-tile %) %) tiles))

(defn read-output [outputs]
  (let [tiles (partition 3 outputs)]
    {:paddle-x (first (find-paddle-tile tiles))
     :ball-x (first (find-ball-tile tiles))
     :score (nth (find-score-tile tiles) 2)
     :tiles (remove is-score-tile tiles)}))

(defn update-vm-state [vm-state {:keys [paddle-x ball-x]}]
  (let [joystick-state (cond (nil? paddle-x) 0
                             (< paddle-x ball-x) 1
                             (> paddle-x ball-x) -1
                             :else 0)]
    (prn joystick-state)
    (-> vm-state
        drop-outputs
        (add-input joystick-state))))

(defn draw-game-state [{:keys [:score :vram]}]
  (reset-terminal)
  (println (vram-to-str vram))
  (println "score:" score))

(defn create-game-state []
  {:paddle-x nil
   :ball-x nil
   :score nil
   :vram empty-vram})

(defn update-vram [{vram :vram :as game-state} tiles]
  (assoc game-state
         :vram
         (reduce draw-tile-to-vram vram tiles)))

(defn not-nil-keys [m ks]
  (->> (select-keys m ks)
       (filter #(not (nil? (second %))))))

(defn update-game-state [game-state outputs]
  (let [new-values (read-output outputs)]
    (-> game-state
        (merge (not-nil-keys new-values [:paddle-x :ball-x :score]))
        (update-vram (:tiles new-values)))))

(defn play-game
  ([vm-state]
   (play-game vm-state (create-game-state)))
  ([vm-state game-state]
   (let [next-vm-state (run-until-needs-input vm-state)
         game-state' (update-game-state game-state
                                        (:outputs next-vm-state))]
     (draw-game-state game-state')
     (comment Thread/sleep 25)
     (if (not (has-block-tiles (:vram game-state')))
       next-vm-state
       (recur (update-vm-state next-vm-state game-state') game-state')))))

(defn -main [& args]
  (let [program (read-intcode-program-from-stdin)
        vm (create-intcode-vm program :memory-size 2659)]
    (prn (->> (run vm)
              :outputs
              (drop 2)
              (take-nth 3)
              (filter #(= 2 %))
              count))
    (play-game (assoc-in vm [:memory 0] 2))))
