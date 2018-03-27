#lang inverse/base

(require inverse/utils
         (only-in racket + - * /))

(provide addn muln subn divn add1 sub1)

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