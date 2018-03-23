#lang s-exp inverse

(require inverse/utils)

(provide addn muln subn divn)

; Number -> [Number <-> Number]
(define (addn n)
  (λ-create-invertible (x)
                       (+ x n)
                       (- x n)))


(define subn (invert-result addn))

; Number <-> Number
(define add1 (addn 1))
(define sub1 (invert add1))

; Number -> [Number <-> Number]
(define (muln n)
  (λ-create-invertible (x)
                       (* x n)
                       (/ x n)))

(define divn (invert-result muln))