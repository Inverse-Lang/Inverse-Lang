#lang inverse/base

(require (for-syntax racket/provide-transform syntax/parse))
(require racket)

(provide invert-result
         noinvert-out
         define-create-invertible
         define-auto-invertible
         create-invertible
         self-invert
         create-adapter
         check-condition)

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

(define (create-adapter adapter-func)
  (λ-create-invertible/defer (adapted)
                             (λ-auto-invertible
                              (arg)
                              (adapter-func (adapted ((invert adapter-func) arg))))
                             (λ-auto-invertible
                              (arg)
                              ((invert adapter-func) (adapted (adapter-func arg))))))

(define-syntax (define-create-invertible stx)
  (syntax-parse stx
    [(_ (name arg) body invbody)
     #`(define name #,(syntax/loc stx (λ-create-invertible (arg) body invbody)))]))

(define-syntax (define-auto-invertible stx)
  (syntax-parse stx
    [(_ (name arg) body)
     #`(define name #,(syntax/loc stx (λ-auto-invertible (arg) body)))]))

; [X Y] [X -> Y] [Y -> X] -> [X <-> Y]
; Creates an invertible function from two other functions
(define (create-invertible func invfunc)
  (λ-create-invertible (x)
                       (func x)
                       (invfunc x)))

; [X] [X -> X] -> [X <-> X]
; Creates an invertible function from two copies of the same function
; (for when a function is an inverse of itself)
(define (self-invert func)
  (create-invertible func func))

(define (check-condition condition [msg "Bounds condition is not met"])
  (self-invert
   (λ (arg)
     (if (condition arg)
         arg
         (raise-arguments-error
          'check-bounds
          msg
          "given" arg)))))



