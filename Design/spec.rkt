; Design Memo
; Melcer, Daniel
; Goldman, Joshua

#lang racket

#|

program = def-expr ...

def-expr = def
         | declare
         | expr


def = (define id expr)
    | (define (id id ...) expr)
    | (define-invertible (id id) expr)

declare = (declare-invertible expr expr)

expr = id
     | datum
     | (expr expr ...)
     | (if expr expr expr)
     | (invert expr)
     | (invertible? expr)
     | (lambda (id ...) expr)
     | (lambda-invertible (id) expr)
     | (lambda-create-invertible (id) expr expr)

datum = Number
      | String
      | Symbol
      | Boolean

Scoping Rules:

(define name expr) binds name in the scope that define is in
(define (name arg ...) body) binds name and all arg in body and
                             binds name in the scope that the define is in
(define-invertible (name arg) body) binds arg in body and
                                    binds name in the scope that the define is in
(lambda (arg ...) body) binds all arg in body
(lambda-invertible (arg) body) binds arg in body
(lambda-create-invertible (arg) body ibody) binds arg in body and ibody

Semantics:
define, id, datum, function application, if, and lambda expression all work
the same as they do in racket

(declare-invertible func1 func2): tells the programming language (in the current scope)
that these functions are inverses of each other. Both functions must accept one
argument and produce one result. It is a runtime error to
declare-invertible a known invertible function

(define-invertible (name arg) body): defines a function as in racket (in the current scope),
with the restriction that body may not contain if, and all function calls with arg
as a parameter must be invertible functions.

(lambda-invertible (arg) body): an anonymous invertible function, similar to a lambda in racket,
with the restriction that body may not contain if, and all function calls with arg
as a parameter must be invertible functions.

(lambda-create-invertible (arg) body ibody): creates an anonymous function, as in racket,
where applying the function is equivalent to applying (lambda (arg) body).
(invert (lambda-create-invertible (arg) body ibody)) is equivalent to:
(lambda-create-invertible (arg) ibody body)

(invert ifunc): ifunc must be an invertible function, returns an invertible function that
is the inverse of ifunc

(invertible? func): returns true if func is an invertible function, false otherwise

EXAMPLES:

(declare-invertible add1 sub1)

(define-invertible (add2 x)
  (add1 (add1 x)))

(define sub2 (invert add2))

(define (add32 x)
  (+ x 32))
(define (sub32 x)
  (- x 32))
(declare-invertible add32 sub32)

(define mul5/9 (lambda-create-invertible (x)
               (* x 5/9)
               (/ x 5/9)))
(define div5/9 (invert mul5/9))

(define-invertible (F->C n)
  (mul5/9 (sub32 n)))

(define C->F (invert F->C))
acts like:
  (add32 (div5/9 n))

(define (addn n)
  (lambda-create-invertible (x)
    (+ x n)
    (- x n)))
(addn 5) -> (λ (x) (+ x 5))
(invert (addn 5)) -> (λ (x) (- x 5))

(define (muln n)
  (lambda-create-invertible (x)
    (* x n)
    (/ x n)))

(define-invertible (F->C* temperature)
  ((muln 5/9) ((addn -32) temperature)))

(define C->F* (invert F->C))

(invertible? add1) -> #t
(invertible? "Hello") -> #f
(invertible? addn) -> #f
(invertible? (addn 42)) ->#t

(define (mul2 x)
  (* x 2))

(define (not-the-inverse x)
  (* x 2))

(declare-invertible ident not-the-inverse)

(ident 0) -> 0
((inverse mul2) 0) -> 0

(mul2 1) -> 2
Will cause an error because:
((inverse mul2) 2) -> 4

|#