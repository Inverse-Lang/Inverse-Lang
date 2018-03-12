#lang racket

(require "function-structs.rkt")
(require (prefix-in un: racket))
(require (for-syntax syntax/parse))

(provide #%module-begin
         (rename-out
          [define-ninv define]
          [lambda-ninv lambda]
          [lambda-ninv λ]
          [apply-func #%app]))

; Define a constant
; Provides a shorthand to define a non-invertible function
; which expands to a lambda
(define-syntax define-ninv
  (syntax-parser
    [(_ (name args ...) body)
     #'(un:define name (lambda-ninv (args ...) body))]
    [(_ name body)
     #'(un:define name body)]))

; Create a non-invertible lambda
(define-syntax lambda-ninv
  (syntax-parser
    [(_ (args ...) body)
     #'(ninvfunc-wrap (un:lambda (args ...) body))]))

; Applies a function, taking it out of its wrapper struct
(define-syntax apply-func
  (syntax-parser
    [(_ func args ...)
     #'(begin (define realfunc
                (cond
                  [(invfunc-wrap? funcy) (invfunc-wrap-func func)]
                  [(ninvfunc-wrap? func) (ninvfunc-wrap-func func)]))
              (un:#%app realfunc args ...))])) 