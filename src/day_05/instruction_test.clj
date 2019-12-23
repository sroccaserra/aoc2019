(ns day-05.instruction-test
  (:require [clojure.test :refer :all]
            [day-05.intcode :refer [create-intcode-vm]]
            [day-05.instruction :refer :all]))

(deftest reading-instructions
  (testing "find opcode"
    (let [vm (create-intcode-vm [99])]
      (is (= {:opcode 99}
             (read-instruction vm)))))

  (testing "read an instruction"
    (let [vm (create-intcode-vm [1 2 4 0 99])]
      (is (= {:opcode 1 :size 4 :parameter-1 4 :parameter-2 99 :dest 0}
             (read-instruction vm))))))
