(define foo (case-lambda ((x) "onearg")
                         ((x y) "twoarg")
                         ((x y z) "threearg")
                         ((x y z s) (foo x))))