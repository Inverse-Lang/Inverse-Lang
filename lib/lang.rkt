#lang racket

(require "./function-structs.rkt")
(require (prefix-in un: racket))
(require (for-syntax syntax/parse))

(provide #%module-begin
         #%datum
         #%top-interaction
         define
         lambda
         λ
         lambda-create-invertible
         invert
         + - / *
         (rename-out
          [apply-func #%app]
          [lambda-create-invertible λ-create-invertible]
          [invfunc-wrap? invertible?]))

; A Function is one of:
(struct invfunc-wrap (func invfunc))

; Create an invertible lambda function
(define-syntax lambda-create-invertible
  (syntax-parser
    [(_ arg body invbody)
     #'(invfunc-wrap (un:lambda (arg) body)
                     (un:lambda (arg) invbody))]))

; Invert an invertible function
(define (invert func)
  (cond
    [(invfunc-wrap? func) (invfunc-wrap (invfunc-wrap-invfunc func)
                                        (invfunc-wrap-func func))]
    [else (error "Not an invertible function")]))

; Applies a function, taking it out of its wrapper struct
(define-syntax apply-func
  (syntax-parser
    [(_ func args ...)
     #'(let ((realfunc
              (cond
                [(invfunc-wrap? func) (invfunc-wrap-func func)]
                [(procedure? func) func])))
         (un:#%app realfunc args ...))])) 