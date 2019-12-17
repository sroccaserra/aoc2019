(ns day-03.path)

(def ^{:private true} increments {'U [0 1]
                                  'D [0 -1]
                                  'L [-1 0]
                                  'R [1 0]})

(def empty-path [[0 0]])

(defn- apply-move [increment path]
  (conj path (map + increment (last path))))

(defn eval-command [path command]
  (let [[direction nb-steps] command
        base-increment (direction increments)
        increment (map (partial * nb-steps) base-increment)]
    (apply-move increment path)))

(defn compute-path [commands]
  (reduce eval-command empty-path commands))
