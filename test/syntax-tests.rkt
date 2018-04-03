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
 (lambda-auto-invertible (x)
                     (+ 1 x)))
(check-syntax-fail
 (lambda-auto-invertible (x)
                     (+ 1 y)))

(check-syntax-fail
 (lambda-auto-invertible (x)
                     (+ (addn x) x)))
(check-expect ((subn 5) 7) 2)

(define c->f (lambda-auto-invertible (x)
                                 ((addn 32) ((muln 9/5) x))))
(define f->c (invert c->f))

(define f->c-adapter (create-adapter f->c))

(define-auto-invertible (raise-100-degrees celsius)
  ((addn 100)
   ((check-condition (λ (x) (> x -273.15))
                  "Cannot go below absolute zero!")
    celsius)))

(define raise-180-farenheit (f->c-adapter raise-100-degrees))
(check-expect (raise-180-farenheit 100) 280)
(check-expect ((invert raise-180-farenheit) 380) 200)
(check-expect (raise-180-farenheit -459) -279)
(check-fail (raise-180-farenheit -460))

(define c->f-adapter (invert f->c-adapter))
(define lower-100-celsius (c->f-adapter (invert raise-180-farenheit)))

(check-expect (lower-100-celsius -173) -273)
(check-fail (lower-100-celsius -300))
;(add1-evil 5)

(check-expect (c->f 100) 212)
(check-expect (f->c 68) 20)



