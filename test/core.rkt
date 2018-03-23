#lang s-exp inverse

(define add1 (λ-create-invertible (num)
                                  (+ num 1)
                                  (- num 1)))

(define sub1 (invert add1))

(define add2 (λ (x)
               (+ x 2)))
(define sub2 (λ (x)
               (- x 2)))

(declare-invertible add2 sub2)

(define add3 (λ (x) (+ x 3)))
(declare-invertible (λ (x) (- x 3)) add3)
(invert add3)
(add3 4)
((invert add3) 4)

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
(define add1-bad (λ-create-invertible (num)
                                      (* num 2)
                                      (- num 2)))

#;(define g (lambda-auto-invert
           (x)
           ((addn (λ (x)(+ x 1))) ((addn 5) x))))
#;(define q (lambda-auto-invert(x) ((muln 2) (g x))))

(c-to-f 0)
(f-to-c 32)
(define add5 (addn 5))