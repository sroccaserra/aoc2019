# Advent of Code 2019

See:

- <https://adventofcode.com/2019>

## Running

Use this command, choosing the day number you want:

    $ lein run-day-01 < resources/day_01/input.txt

## Running tests

    $ lein test-refresh


## Learnings

See also:
- <https://github.com/sroccaserra/aoc2015#learnings>
- <https://github.com/sroccaserra/aoc2018#learnings>
- <https://github.com/sroccaserra/aoc2020#learnings>
- <https://github.com/sroccaserra/aoc2021#learnings>
- <https://github.com/sroccaserra/aoc2022#learnings>

### General

- Using `stdin` instead of a file is handy: I can use `echo` to easily pass
  various made up input to the `main` function.

- If I have a strictly monotone function _f(x) = y_ that is hard to invert, to
  compute the amount of _x_ provided by a large amount of _y_ I can use binary
  search (see [day 14 code](src/day_14/Day14.hs), `f(fuel) = ore`).

- The upper triangular matrix with only ones can be constructed by summing I and powers of J (J being the matrix with ones only above the diagonal)

```
[ 1 1 1 1 ]
[ 0 1 1 1 ] = I + J + J^2 + J^3
[ 0 0 1 1 ]
[ 0 0 0 1 ]

with:

[ 0 1 0 0 ]
[ 0 0 1 0 ] = J
[ 0 0 0 1 ]
[ 0 0 0 0 ]
```

- The nth diagonal of Pascal's Triangle can be computed iteratively: _x<sub>k</sub> = x<sub>k-1</sub>*(n+k)/k_
    - <https://en.wikipedia.org/wiki/Pascal%27s_triangle>
- There is a Pascal Matrix:
    - <https://en.wikipedia.org/wiki/Pascal_matrix>
- Fermat's little theorem is useful to compute the modular inverse, see [day 22](src/day_22/main.py) comments

### Clojure

- Small clojure setup:
    - an `src/toto.clj` file, with a `(ns toto)` declaration and a `(defn -main [& args] (println "salut"))` function
    - run with `clj -M -m toto`

- `clj` REPL starts faster than `lein repl`

- For fast feedback, start a REPL and don't close it. Change the file and reload it, with `(do (use 'day-21.main :reload) (-main))` for example. Not working the first time it seems?

- [fn][fn] defines a recursion point, so I don't need to `loop` to `recur` a
  `fn` (also true for `defn`).

- [Destructuring][destruct] is powerfull in Clojure.

- Clojure has `:pre` and `:post` conditions. See: [fn][fn].

- We can substitute functions, during a test execution for instance, with [with-redefs][with-redefs].

- I can access the Clojure doc from the REPL: [doc][doc], [find-doc][find-doc], [apropos][apropos].

- I can use [doto][doto] as a kind of [tap][tap] or [kestrel][kestrel].

- I can use [run!][run!] to print each item in a lazy collection on a new line.

### Haskell

- In `Data.Array`, `listArray` can construct an array using its previous values (sort of recrusively):

```haskell
fibs :: Array Int Integer
fibs = listArray (0, n-1) $ 0 : 1 : [fibs!(i-1) + fibs!(i-2) | i <- [2..n-1]]
  where n = 100
```

### Python

- For a defaultdict, `tuple(sorted(a_defaultdict.items()))` can be inserted in a set (to build a history of states)
- For a set, `tuple(sorted(a_set))` can be inserted in a set (to build a history of states)

### References

- <https://github.com/tpope/vim-fireplace>
- <https://github.com/jakemcc/lein-test-refresh>
- <https://clojure.github.io/clojure/clojure.test-api.html>
- <https://stuartsierra.com/2015/08/25/clojure-donts-lazy-effects>

[fn]: https://clojure.org/reference/special_forms#fn
[destruct]: https://clojure.org/guides/destructuring
[with-redefs]: https://clojuredocs.org/clojure.core/with-redefs
[doc]: https://clojuredocs.org/clojure.repl/doc
[find-doc]: https://clojuredocs.org/clojure.repl/find-doc
[apropos]: https://clojuredocs.org/clojure.repl/apropos
[doto]: https://clojuredocs.org/clojure.core/doto
[tap]: https://ruby-doc.org/core-2.1.5/Object.html#method-i-tap
[kestrel]: https://github.com/raganwald-deprecated/homoiconic/blob/master/2008-10-29/kestrel.markdown#readme
[run!]: https://clojuredocs.org/clojure.core/run!
