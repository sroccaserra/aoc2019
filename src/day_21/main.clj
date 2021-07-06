(ns day-21.main
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-common-cli :refer [read-intcode-program-from-file]]
            [intcode.vm-state :refer [create-intcode-vm add-inputs write-int-at]]
            [intcode.run :refer [run run-until-needs-input]]))

; @
; #ABCDEFGHI

; @
; #.########

; @
; #..#######

; not a j
; @
; #...######
; @
; #.#.#...##
;  ABCDEFGHI

; not c t and d t and i t
; @
; ###.#..###
;  ABCDEFGHI


; not c t and d t and h t
; @
; ###.#...#.#
;  ABCDEFGHI

; prevent this:
; @
; ###.#.##.#
;  ABCDEFGHI

; not b t and d t
; @
; ##.##.####
;  ABCDEFGHI

(def program "NOT C J
             AND D J
             AND I J
             NOT F T
             AND T J
             NOT A T
             OR T J
             NOT C T
             AND D T
             AND H T
             OR T J
             NOT B T
             AND D T
             OR T J
             RUN
             ")

(defn -main [& args]
  (let [intcode-program (read-intcode-program-from-file "resources/day_21/input.txt")
        vm (create-intcode-vm intcode-program
                              :inputs (map int program)
                              :memory-size 4000)]
          (->> vm run :outputs (map char) (apply str) print)))
