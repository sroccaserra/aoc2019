(ns day-11.canvas-and-robot-test
  (:require [clojure.test :refer :all]
            [day-11.canvas-and-robot :refer :all]))

(deftest testing-canvas-and-robot
  (testing "creation"
    (is (= {:canvas {{:x 0 :y 0} black} :robot {:x 0 :y 0 :direction \^}}
           (create-canvas-and-robot black))))

  (testing "update"
    (let [c-and-r (create-canvas-and-robot black)]
      (is (= {:canvas {{:x 0 :y 0} white} :robot {:x -1 :y 0 :direction \<}}
             (update-canvas-and-robot c-and-r [1 0])))))

  (testing "update n times"
    (let [c-and-r (-> (create-canvas-and-robot black)
                      (update-canvas-and-robot [1 0])
                      (update-canvas-and-robot [1 0])
                      (update-canvas-and-robot [1 0])
                      (update-canvas-and-robot [1 0]))]
      (is (= {:canvas {{:x 0 :y 0} white
                       {:x -1 :y 0} white
                       {:x -1 :y -1} white
                       {:x 0 :y -1} white} :robot {:x 0 :y 0 :direction \^}}
             c-and-r))))

  (testing "read color"
    (let [c-and-r  (create-canvas-and-robot black) ]
      (is (= black
             (read-robot-color c-and-r)))))

  (testing "read color after four updates"
    (let [c-and-r (-> (create-canvas-and-robot black)
                      (update-canvas-and-robot [1 0])
                      (update-canvas-and-robot [1 0])
                      (update-canvas-and-robot [1 0])
                      (update-canvas-and-robot [1 0]))]
      (is (= white
             (read-robot-color c-and-r))))))
