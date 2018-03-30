#lang racket

(require syntax/location)

(module x racket
  (define foo 5)
  (provide foo))

(define path (quote-module-path x))

(define ns (make-base-empty-namespace))

(parameterize ([current-namespace ns])
  (namespace-require path))
(eval 'foo ns)

(define x (make-parameter #f))

(define (f)
  (x))

(define (g)
  (f))

(parameterize ([x 8])
  (g))