#lang inverse/base


(require (only-in racket string-append substring string-length -
                  number->string string->number rename-out))
(require inverse/utils)
(provide prepend-str append-str
         (rename-out [num->str number->string]
                     [str->num string->number]))

; String -> [String <-> String]
(define (prepend-str str)
  (λ-create-invertible (x)
                       (string-append str x)
                       (substring x (string-length str))))

; String -> [String <-> String]
(define (append-str str)
  (λ-create-invertible (x)
                       (string-append x str)
                       (substring x 0 (- (string-length x) (string-length str)))))

(define num->str
  (λ-create-invertible (x)
                       (number->string x)
                       (string->number x)))
(define str->num (invert num->str))
