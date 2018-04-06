#lang inverse

(define add5 (addn 5))
(define sub5 (invert add5))

(define c->f (lambda-auto-invertible (x)
                                     ((addn 32) ((muln 9/5) x))))

(define-auto-invertible (raise-100-celsius degrees)
  ((addn 100) degrees))

(define c->f-adapter (create-adapter c->f))

(define raise-180-farenheit (c->f-adapter raise-100-celsius))

(define n->s-adapter (create-adapter number->string))
(define add1-string (n->s-adapter add1))

(define (addn-string n)
  (λ-auto-invertible (x)
                     ((n->s-adapter
                       (addn (string->number n))) x)))

(define subn-string (invert-result addn-string))

;(invert (addn-string x)) = ((invert-result addn-string) x)
