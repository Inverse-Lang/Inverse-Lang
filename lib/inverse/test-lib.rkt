#lang racket

(require rackunit)
(require (for-syntax rackunit))
(require (for-syntax syntax/parse))

(provide
 (rename-out [check-equal? check-expect])
 check-syntax-fail
 check-fail)

(define-syntax check-syntax-fail
  (syntax-parser
    [(_ syn)
     (check-exn exn:fail:syntax?
                (λ()
                  (syntax-local-expand-expression #'syn)))
     #'(void)]))

(check-syntax-fail (λ () x))

(define-syntax check-fail
  (syntax-parser
    [(_ syn)
     #'(check-exn exn:fail? (λ () syn))]))