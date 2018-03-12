#lang s-exp "../lib/lang.rkt"

(define add1 (λ-create-invertible num
                                  (+ num 1)
                                  (- num 1)))

(define sub1 (invert add1))