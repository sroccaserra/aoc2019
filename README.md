# Advent of Code 2019

See:

- <https://adventofcode.com/2019>

## Running

Use this command, choosing the day number you want:

    $ lein run-day-01 < resources/day_01/input.txt

## Running tests

    $ lein test-refresh


## What I learned

### General

- Using `stdin` instead of a file is handy: I can use `echo` to easily pass
  various made up input to the `main` function.

- If I have a strictly monotone function _f(x) = y_ that is hard to invert, to
  compute the amount of _x_ provided by a large amount of _y_ I can use binary
  search (see [day 14 code](src/day_14/Day14.hs), `f(fuel) = ore`).

### Clojure

- [fn][fn] defines a recursion point, so I don't need to `loop` to `recur` a
  `fn` (also true for `defn`).

- [Destructuring][destruct] is powerfull in Clojure.

- Clojure has `:pre` and `:post` conditions. See: [fn][fn].

- We can substitute functions, during a test execution for instance, with [with-redefs][with-redefs].

- I can access the Clojure doc from the REPL: [doc][doc], [find-doc][find-doc], [apropos][apropos].

- I can use [doto][doto] as a kind of [tap][tap] or [kestrel][kestrel].

- I can use [run!][run!] to print each item in a lazy collection on a new line.

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
