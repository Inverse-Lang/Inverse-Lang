#lang racket

(require (prefix-in un: racket))
(require (for-syntax syntax/parse)
         (for-syntax syntax/quote))
(require racket/require)

(module reader syntax/module-reader
  inverse/base
  #:read
  read
  #:read-syntax
  read-syntax)

(provide
 lambda-create-invertible
 lambda-create-invertible!
 lambda-auto-invertible
 invert
 declare-invertible
 #%module-begin
 #%datum
 #%top
 #%top-interaction
 #%app
 module
 require
 provide
 prefix-in
 all-from-out
 for-syntax
 define
 λ
 lambda
 only-in
 except-out
 quote
 subtract-in
 (rename-out
  [lambda-create-invertible λ-create-invertible]
  [lambda-create-invertible! λ-create-invertible!]
  [invfunc-wrap? invertible?]
  [lambda-auto-invertible λ-auto-invertible]
  [invfunc-wrap-func noinvert]))

; A Function is one of:
(struct invfunc-wrap (func invfunc verify-inverse defer check-also) #:transparent
  #:property prop:procedure
  (λ (func arg)
    (define result ((invfunc-wrap-func func) arg))
    (define result-inv ((invfunc-wrap-invfunc func) result))
    (if (and (invfunc-wrap-verify-inverse func)
             (not (invfunc-wrap-defer func))
             (not (eqv? arg result-inv)))
        (raise-arguments-error
         'invertible-check
         "Not a true invertible function: given argument and inverse applied to result must match."
         "given argument" arg "result" result "inverse applied to result" result-inv)
        result)))

(define inverse-context (make-parameter '()))

(define test-deferred-inverrtible
  (λ (func2 invfunc2)
    (λ (func1)
      (λ (x)
        (define result-of-inv ((invfunc2 (func2 func1)) x))
        (define result (func1 x))
        (if (not (equal? result result-of-inv))
            (raise-arguments-error
             'deferred-invertible-check
             "Deferred function not true invertible: "
             "non-inverted result" result
             "inverted result" result-of-inv)
            (void))))))

; Create an invertible lambda function
(define-syntax (lambda-create-invertible stx)
  (syntax-parse stx
    [(_ (arg) body invbody)
     #`(invfunc-wrap #,(syntax/loc stx (un:lambda (arg) body))
                     #,(syntax/loc stx (un:lambda (arg) invbody))
                     #t #f '())]))

(define-syntax (lambda-create-invertible/d stx)
  (syntax-parse stx
    [(_ (arg) body invbody)
     #`(begin
         (define func #,(syntax/loc stx (un:lambda (arg) body)))
         (define invfunc #,(syntax/loc stx (un:lambda (arg) invbody)))
         (define deferred-func-test (test-deferred-invertible func invfunc))
         (define deferred-invfunc-test (test-deferred-invertible invfunc func))
         (invfunc-wrap #,(syntax/loc stx
                           (un:lambda (arg) (parameterize [(inverse-context
                                                            (deferred-func-test arg))] body))
                           (un:lambda (arg) (parameterize [(inverse-context
                                                            (deferred-invfunc-test arg))] invbody))
                           #f #t '())])))))]))

(define-syntax (lambda-create-invertible! stx)
  (syntax-parse stx
    [(_ (arg) body invbody)
     #`(invfunc-wrap #,(syntax/loc stx (un:lambda (arg) body))
                     #,(syntax/loc stx (un:lambda (arg) invbody))
                     #f #f '())]))

; Create a function composed of other invertible functions
; Automatically construct the inverse
(define-syntax (lambda-auto-invertible stx)
  (syntax-parse stx
    [(_ (arg) body)
     (syntax/loc stx
       (lambda-create-invertible
        (arg)
        body
        (construct-inverse arg body arg)))]))

(define-for-syntax get-innermost
  (syntax-parser
    [(_ (invertiblefunc funcarg))
     (get-innermost #'funcarg)]
    [(_ funcarg)
     (displayln #'funcarg)
     #'funcarg]))

(define (proc-takes-one-arg func)
  (and (procedure? func) (procedure-arity-includes? func 1)))

(define-for-syntax decl-top-err "Declarations must be at top level")

(define-syntax declare-invertible
  (syntax-parser
    [(_ func1:id func2:id)
     #:fail-unless (equal? (syntax-local-context) 'module) decl-top-err
     #'(if (and (proc-takes-one-arg func1)
                (proc-takes-one-arg func2))
           (begin (set! func1 (invfunc-wrap func1 func2))
                  (set! func2 (invert func1)))
           (error "Cannot declare these invertible"))]
    [(_ func1:id func2)
     #:fail-unless (equal? (syntax-local-context) 'module) decl-top-err
     #'(if (and (proc-takes-one-arg func1)
                (proc-takes-one-arg func2))
           (set! func1 (invfunc-wrap func1 func2))
           (error "Cannot declare these invertible"))]
    [(_ func1 func2:id)
     #:fail-unless (equal? (syntax-local-context) 'module) decl-top-err
     #'(declare-invertible func2 func1)]))
  
; Construct the inverse of the body of a function
; ACCUMULATOR: inner represents the inverses of outer function calls
(define-syntax construct-inverse
  (syntax-parser
    [(_ inner (invertiblefunc ifuncarg) correctarg)
     #:fail-unless (not (contains (local-expand #'invertiblefunc 'expression '()) #'correctarg))
     "Argument to auto-invertible function can only be used in the innermost function call"
     #'(construct-inverse ((invert invertiblefunc) inner) ifuncarg correctarg)]
    [(_ inner arg:id correctarg:id)
     #:fail-unless (free-identifier=? #'arg #'correctarg)
     (format
      "Expected ~a, got ~a. "
      (syntax->datum #'correctarg) (syntax->datum #'arg))
     #'inner]))

(define-for-syntax (contains stx id-to-look-for) 
  (syntax-parse stx
    [(stuff ...) (ormap
                  (lambda (sx) (contains sx id-to-look-for))
                  (syntax->list #'(stuff ...)))]
    [stuff                
     (and (identifier? #'stuff) (free-identifier=? #'stuff id-to-look-for))]))

; Invert an invertible function
(define (invert func)
  (cond
    [(invfunc-wrap? func) (invfunc-wrap (invfunc-wrap-invfunc func)
                                        (invfunc-wrap-func func)
                                        (invfunc-wrap-verify-inverse func))]
    [else (error "Not an invertible function")]))
