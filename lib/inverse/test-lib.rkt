#lang racket

(require rackunit)
(require (for-syntax rackunit))
(require (for-syntax syntax/parse))
(require (prefix-in inv: inverse/base))

(provide
 (rename-out [check-equal? check-expect])
 check-syntax-fail
 check-fail)

; Does a given expression throw a syntax error?
(define-syntax check-syntax-fail
  (syntax-parser
    [(_ syn)
     (check-exn exn:fail:syntax?
                (λ()
                  (syntax-local-expand-expression #'syn)))
     #'(void)]))

(check-syntax-fail (λ () x))

; Is an exception produced by the given expression?
(define-syntax (check-fail stx)
  (syntax-parse stx
    [(_ syn)
     (syntax/loc stx (check-exn exn:fail? (λ () syn)))]))