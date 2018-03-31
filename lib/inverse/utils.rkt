#lang inverse/base

(require (for-syntax racket/provide-transform syntax/parse))
(require racket)

(provide invert-result noinvert-out)

; [X Y Z] [X -> [Y <-> Z]] -> [X -> [Z <-> Y]]
; Given a function that produces another function,
; invert the result of a function
(define (invert-result func)
  (λ (input)
    (invert (func input))))

; Breaks out a function and its inverse into two separate imports
(define-syntax noinvert-out
  (make-provide-pre-transformer
   (lambda (stx modes)
     (syntax-parse stx
       [(_ invertible funcout invfuncout)
        #:with fout (syntax-local-lift-expression
                     #'(noinvert invertible))
        #:with invfout (syntax-local-lift-expression
                        #'(noinvert (invert invertible)))
        (pre-expand-export
         #'(rename-out [fout funcout]
                       [invfout invfuncout])
         modes)]))))

