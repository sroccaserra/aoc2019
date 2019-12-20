(ns day-04.password)

(defn has-two-identical-adjacent-numbers [digits]
  (or (= (nth digits 0) (nth digits 1))
      (= (nth digits 1) (nth digits 2))
      (= (nth digits 2) (nth digits 3))
      (= (nth digits 3) (nth digits 4))
      (= (nth digits 4) (nth digits 5))))

(defn digits-never-decrease [digits]
  (and (<= (nth digits 0) (nth digits 1))
       (<= (nth digits 1) (nth digits 2))
       (<= (nth digits 2) (nth digits 3))
       (<= (nth digits 3) (nth digits 4))
       (<= (nth digits 4) (nth digits 5))))

(defn number->digits [n]
  (->> n
       str
       (map (comp read-string str))))

(defn count-possible-passwords-in-range []
  (count (filter #(and (has-two-identical-adjacent-numbers %)
                       (digits-never-decrease %))
                 (map number->digits (range 138241 (inc 674034))))))
