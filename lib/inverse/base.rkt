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
 lambda-create-invertible/defer
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
  [lambda-create-invertible/defer λ-create-invertible/defer]
  [invfunc-wrap? invertible?]
  [lambda-auto-invertible λ-auto-invertible]
  [invfunc-wrap-func noinvert]))

; A Test is a (list InvFunction InvFunction)
; For the test to pass, both procedures must produce the same result when
; applied to a given argument

; A InvFunction a:
;(invfunc-wrap procedure procedure boolean [List-of Test])

(define (test-cascade tests arg)
  #; (if (not (empty? tests))
         (and (displayln tests)
              (displayln arg))
         (void))
  (andmap
   (λ (test)
     (with-handlers ([exn:fail? (λ (exn) (displayln (exn-message exn)) #f)])
       (define res1 ((invfunc-wrap-func (first test)) arg))
       ;(displayln res1)
       (define res2 ((invfunc-wrap-func (second test)) arg))
       ; (displayln res2)
       (if (equal? res1 res2)
           #t
           (begin ((displayln (format "Deferred invertible check FAILED. Actual: ~a. Expected: ~a."
                                      res1
                                      res2)))
                  #f))))
   tests))

(define (cascade-apply tests arg)
  (map (λ (test) (list ((first test) arg)
                       ((second test) arg)))
       tests))


(define (invert-cascade tests)
  ;(if (not (empty? tests)) (displayln tests) (void))
  (map (λ (test)
         (list (invert (first test))
               (invert (second test))))
       tests))

(define current-cascade (make-parameter '()))

(struct invfunc-wrap (func invfunc verify-inverse cascaded-tests) #:transparent
  #:property prop:procedure
  (λ (func arg)
    (if (and (invfunc-wrap-verify-inverse func)
             (not (test-cascade (invfunc-wrap-cascaded-tests func) arg)))
        (raise-arguments-error
         'cascade-check
         (string-append "A deferred invertible test has failed. " 
                        "If this function was produced as the result of an invertible function, "
                        "the function that produced this was not itself invertible")
         "given argument" arg)
        (void))
    (define next-cascade (if (invfunc-wrap-verify-inverse func) '()
                             (cascade-apply (invfunc-wrap-cascaded-tests func) arg)))
    (define result
      (parameterize [(current-cascade next-cascade)]
        ((invfunc-wrap-func func) arg)))
    (define result-inv ((invfunc-wrap-invfunc func) result))
    (if (and (invfunc-wrap-verify-inverse func)
             (not (eqv? arg result-inv)))
        (raise-arguments-error
         'invertible-check
         "Not a true invertible function: given argument and inverse applied to result must match."
         "given argument" arg "result" result "inverse applied to result" result-inv)
        result)))

; Create an invertible lambda function
(define-syntax (lambda-create-invertible stx)
  (syntax-parse stx
    [(_ (arg) body invbody)
     #`(invfunc-wrap #,(syntax/loc stx (un:lambda (arg) body))
                     #,(syntax/loc stx (un:lambda (arg) invbody))
                     #t
                     (current-cascade))]))

(define-syntax (lambda-create-invertible/defer stx)
  (syntax-parse stx
    [(_ (arg) body invbody)
     #`(begin (define prev-cascade (current-cascade))
              (define cascader-function (invfunc-wrap
                                         #,(syntax/loc stx (un:lambda (arg) body))
                                         #,(syntax/loc stx (un:lambda (arg) invbody))
                                         #f
                                         '()))
              (define new-test1
                (parameterize [(current-cascade '())]
                  (lambda-auto-invertible! (ar) ((invert cascader-function) (cascader-function ar)))))
              (define new-test2
                (parameterize [(current-cascade '())]
                  (lambda-auto-invertible! (ar) ar)))
              (define new-cascade (cons (list new-test1 new-test2) prev-cascade))
              (invfunc-wrap
               #,(syntax/loc stx (un:lambda (arg) body))
               #,(syntax/loc stx (un:lambda (arg) invbody))
               #f
               new-cascade))]))

(define-syntax (lambda-create-invertible! stx)
  (syntax-parse stx
    [(_ (arg) body invbody)
     #`(invfunc-wrap #,(syntax/loc stx (un:lambda (arg) body))
                     #,(syntax/loc stx (un:lambda (arg) invbody))
                     #f
                     '())]))

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

; Create a function composed of other invertible functions
; Automatically construct the inverse
(define-syntax (lambda-auto-invertible! stx)
  (syntax-parse stx
    [(_ (arg) body)
     (syntax/loc stx
       (lambda-create-invertible!
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
                                        (invfunc-wrap-verify-inverse func)
                                        (invert-cascade (invfunc-wrap-cascaded-tests func)))]
    [else (error "Not an invertible function")]))
