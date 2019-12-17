(ns day-03.path-test
  (:require [clojure.test :refer :all]
            [day-03.path :refer :all]))

(deftest evaluating-command-tests
  (testing "evaluating commands from start position"
    (is (= [[0 0] [0 1]]
           (eval-command empty-path '(U 1))))

    (is (= [[0 0] [0 2]]
           (eval-command empty-path '(U 2))))

    (is (= [[0 0] [0 -2]]
           (eval-command empty-path '(D 2))))

    (is (= [[0 0] [-2 0]]
           (eval-command empty-path '(L 2))))

    (is (= [[0 0] [2 0]]
           (eval-command empty-path '(R 2)))))

  (testing "evaluating a list of commands"
    (is (= [[0 0] [0 1] [1 1]]
           (compute-path '((U 1) (R 1)))))))

(deftest find-crossings
  (testing "building all points for vertical segments"
    (is (= #{[0 0] [0 1]}
           (all-points-between [[0 0] [0 1]])))
    (is (= #{[0 0] [0 1] [0 2]}
           (all-points-between [[0 0] [0 2]]))))

(testing "building all points for horizontal segments"
    (is (= #{[0 0] [1 0]}
           (all-points-between [[0 0] [1 0]])))
    (is (= #{[0 0] [1 0] [2 0]}
           (all-points-between [[0 0] [2 0]]))))

(testing "is segment vertical"
  (is (= true (is-segment-vertical [[0 0] [0 1]])))))

(deftest computing-complex-paths
  (testing "computing a few long paths"
    (is (= 4
           (count (compute-path '((R 990) (U 796) (U 604)))))))

  (testing "computing many long paths"
    (is (= 302
           (count (compute-path '((R 990) (U 796) (R 784) (U 604) (R 6) (U 437) (L 96) (U 285) (L 361) (U 285) (L 339) (D 512) (L 389) (D 840) (L 425) (U 444) (L 485) (D 528) (L 262) (U 178) (L 80) (U 2) (R 952) (U 459) (L 361) (D 985) (R 56) (U 135) (R 953) (D 913) (L 361) (U 120) (L 329) (U 965) (L 294) (U 890) (L 126) (U 214) (R 232) (D 444) (L 714) (U 791) (R 888) (U 923) (R 378) (U 233) (L 654) (D 703) (R 902) (D 715) (R 469) (D 60) (R 990) (U 238) (R 755) (U 413) (L 409) (D 601) (R 452) (U 504) (R 472) (D 874) (L 766) (D 594) (R 696) (U 398) (R 593) (D 889) (R 609) (D 405) (L 962) (U 176) (L 237) (U 642) (L 393) (D 91) (L 463) (U 936) (R 199) (D 136) (R 601) (D 8) (R 359) (D 863) (L 410) (U 598) (L 444) (D 34) (R 664) (D 323) (R 72) (D 98) (L 565) (D 476) (L 197) (D 132) (R 510) (U 665) (R 936) (U 3) (R 385) (U 144) (L 284) (D 713) (L 605) (U 106) (R 543) (D 112) (R 528) (D 117) (R 762) (U 330) (R 722) (U 459) (L 229) (U 375) (L 870) (D 81) (R 623) (U 95) (L 148) (D 530) (L 622) (D 62) (R 644) (D 365) (L 214) (U 847) (R 31) (D 832) (L 648) (D 293) (R 79) (D 748) (L 270) (U 159) (L 8) (U 83) (R 195) (U 912) (L 409) (D 649) (L 750) (D 286) (L 623) (D 956) (R 81) (U 775) (R 44) (D 437) (L 199) (U 698) (L 42) (U 419) (L 883) (U 636) (L 323) (U 89) (L 246) (D 269) (L 992) (U 739) (R 62) (U 47) (R 63) (U 17) (L 234) (U 135) (R 126) (D 208) (L 69) (U 550) (L 123) (D 66) (R 463) (U 992) (R 411) (D 276) (L 851) (U 520) (R 805) (D 300) (L 894) (U 171) (L 922) (D 901) (R 637) (U 907) (R 328) (U 433) (L 316) (D 644) (L 398) (U 10) (L 648) (D 190) (R 884) (U 474) (R 397) (D 718) (L 925) (D 578) (R 249) (U 959) (L 697) (D 836) (R 231) (U 806) (R 982) (U 827) (R 579) (U 830) (L 135) (D 666) (R 818) (D 502) (L 898) (D 585) (R 91) (D 190) (L 255) (U 535) (R 56) (U 390) (R 619) (D 815) (L 300) (D 81) (R 432) (D 70) (L 940) (D 587) (L 259) (D 196) (R 241) (U 4) (R 440) (U 678) (R 185) (U 451) (R 733) (D 984) (R 464) (D 298) (L 738) (U 600) (R 353) (D 44) (L 458) (U 559) (L 726) (D 786) (L 307) (D 333) (L 226) (D 463) (R 138) (D 142) (L 521) (D 201) (R 51) (D 202) (L 204) (U 130) (L 333) (U 597) (R 298) (U 42) (L 951) (U 66) (R 312) (U 707) (L 555) (D 225) (L 360) (D 12) (L 956) (D 361) (L 989) (D 625) (L 944) (D 398) (L 171) (D 982) (L 377) (U 114) (L 339) (U 164) (R 39) (D 793) (R 992) (U 834) (R 675) (U 958) (R 334) (D 697) (L 734) (D 40) (L 149) (U 394) (R 976))))))))
