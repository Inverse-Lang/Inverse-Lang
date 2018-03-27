#lang inverse

(check-syntax-fail
 (declare-invertible 5 7))

(check-expect (add1 5) 6)
(check-expect (sub1 7) 6)
(check-expect (invert add1) sub1)

(define add1-evil (λ-create-invertible (x)
                                       (+ x 1)
                                       (* x 0)))

(check-expect (add1-evil 0) 1)
(check-expect ((invert add1-evil) 1) 0)
(check-fail (add1-evil 5))
(check-fail ((invert add1-evil) 5))

(check-syntax-fail
 (lambda-auto-invert (x)
                     (+ 1 x)))
(check-syntax-fail
 (lambda-auto-invert (x)
                     (+ 1 y)))

(check-syntax-fail
 (lambda-auto-invert (x)
                     (+ (addn x) x)))
(check-expect ((subn 5) 7) 2)

(define c->f (lambda-auto-invert (x)
                                 ((addn 32) ((muln 9/5) x))))
(define f->c (invert c->f))

(check-expect (c->f 100) 212)
(check-expect (f->c 68) 20)