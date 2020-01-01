(ns day-11.main
  (:gen-class)
  (:require [aoc-common-cli :refer [read-intcode-program-from-stdin]]
            [intcode.vm-state :refer [create-intcode-vm halted? drop-outputs add-input]]
            [intcode.run :refer [run-until-needs-input]]
            [day-11.canvas-and-robot :refer :all]))

(defn paint-canvas [c-and-r vm]
  (if (halted? vm)
    c-and-r
    (let [vm' (run-until-needs-input vm)
          command (:outputs vm')
          c-and-r' (update-canvas-and-robot c-and-r command)
          color (read-robot-color c-and-r')]
      (recur c-and-r' (-> vm' drop-outputs (add-input color))))))

(defn -main [& args]
  (let [program (read-intcode-program-from-stdin)
        c-and-r (create-canvas-and-robot)
        vm (create-intcode-vm program :inputs [0] :memory-size 1024)]
    (-> (paint-canvas c-and-r vm)
        :canvas
        count
        prn)))
