(defproject advent-of-code-2019 "0.1.0-SNAPSHOT"
  :description "My late and lazy attempt at AoC 2019"
  :url "https://github.com/sroccaserra/advent-of-code-2019"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/core.match "0.3.0"]]
  :profiles {:dev {:plugins [[com.jakemccrary/lein-test-refresh "0.24.1"]]}
             :day-01 {:main advent-of-code-2019.day-01.main}
             :day-02 {:main advent-of-code-2019.day-02.main}}
  :aliases {"run-day-01" ["with-profile" "day-01" "run"]
            "run-day-02" ["with-profile" "day-02" "run"]})
