#lang s-exp "../lib/lang.rkt"

(define add1 (λ-create-invertible (num)
                                  (+ num 1)
                                  (- num 1)))

(define sub1 (invert add1))

(define addn (λ (n)
               (λ-create-invertible (x)
                                    (+ x n)
                                    (- x n))))
(define muln (λ (n)
               (λ-create-invertible (x)
                 (* x n)
                 (/ x n))))

(define c-to-f (lambda-auto-invert
                (x)
                ((addn 32) ((muln 9/5) x))))
(define f-to-c (invert c-to-f))

(define g (lambda-auto-invert
           (x)
           ((addn -4) ((addn 5) x))))
(define q (lambda-auto-invert(x) ((muln 2) (g x))))

(c-to-f 0)
(f-to-c 32)
(q 5)
((invert q) 12)
(define add5 (addn 5))