(defproject advent-of-code-2019 "0.1.0-SNAPSHOT"
  :description "My late and lazy attempt at AoC 2019"
  :url "https://github.com/sroccaserra/advent-of-code-2019"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]]

  :profiles {:dev {:plugins [[com.jakemccrary/lein-test-refresh "0.24.1"]]}
             :day-01 {:main day-01.main}
             :day-02 {:main day-02.main}
             :day-03 {:main day-03.main}}

  :test-paths ["src"]

  :aliases {"run-day-01" ["with-profile" "day-01" "run"]
            "run-day-02" ["with-profile" "day-02" "run"]
            "run-day-03" ["with-profile" "day-03" "run"]})
