#lang inverse

(provide prepend-str append-str)

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
  