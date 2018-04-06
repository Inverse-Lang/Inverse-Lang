#lang inverse

(define evil
  (λ-create-invertible (x)
                       (+ x 1)
                       (* x 0)))

#;(define interesting-auto
  (λ-auto-invertible (x)
                     ((addn x) x)))

(define bar (λ(x) x))

(define baz (λ-auto-invertible (x)
                   (bar x)))

(define (foo throwaway)
  (λ-auto-invertible (x) x))

(define interesting-auto2
  (λ-auto-invertible (x)
                     ((foo (λ (x) x)) x)))