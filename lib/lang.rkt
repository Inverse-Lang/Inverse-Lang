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
         lambda-auto-invert
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
    [(_ (arg) body invbody)
     #'(invfunc-wrap (un:lambda (arg) body)
                     (un:lambda (arg) invbody))]))

; Create a function composed of other invertible functions
; Automatically construct the inverse
(define-syntax lambda-auto-invert
  (syntax-parser
    [(_ (arg) body)
     #`(lambda-create-invertible
        (arg)
        body
        (construct-inverse arg body arg))]))

(define-for-syntax get-innermost
  (syntax-parser
    [(_ (invertiblefunc funcarg))
     (get-innermost #'funcarg)]
    [(_ funcarg)
     (displayln #'funcarg)
     #'funcarg]))
  
; Construct the inverse of the body of a function
; ACCUMULATOR: inner represents the inverses of outer function calls
(define-syntax construct-inverse
  (syntax-parser
    [(_ inner (invertiblefunc ifuncarg) correctarg)
     #'(construct-inverse (apply-func (invert invertiblefunc) inner) ifuncarg correctarg)]
    [(_ inner arg correctarg)
     #:fail-unless (equal? (syntax-e #'arg) (syntax-e #'correctarg))
     (format
      "Expected ~a, got ~a. "
      (syntax->datum #'correctarg) (syntax->datum #'arg))
     #'inner]))
     

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