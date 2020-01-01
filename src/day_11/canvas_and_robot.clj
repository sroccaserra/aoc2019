(ns day-11.canvas-and-robot)

(def black 0)
(def white 1)

(def turns {0 :left
            1 :right})

(def directions {\^ {:dx 0 :dy 1 :left \< :right \>}
                 \v {:dx 0 :dy -1 :left \> :right \<}
                 \< {:dx -1 :dy 0 :left \v :right \^}
                 \> {:dx 1 :dy 0 :left \^ :right \v}})

(defn turn-robot [{direction :direction :as robot} turn]
  (assoc robot :direction (get-in directions [direction turn])))

(defn advance-robot [{x :x y :y direction :direction
                      :as robot}]
  (let [{:keys [dx dy]} (directions direction)]
    (assoc robot :x (+ x dx) :y (+ y dy))))

(defn read-robot-color [{{:keys [x y]} :robot canvas :canvas}]
  (get canvas {:x x :y y} black))

(defn create-canvas-and-robot []
  {:canvas {}
   :robot {:x 0 :y 0 :direction \^}})

(defn update-canvas-and-robot [{robot :robot :as c-and-r} [color turn]]
  (let [point (select-keys robot [:x :y])]
    (-> c-and-r
        (assoc-in [:canvas point] color)
        (assoc :robot (-> robot
                          (turn-robot (turns turn))
                          advance-robot)))))
