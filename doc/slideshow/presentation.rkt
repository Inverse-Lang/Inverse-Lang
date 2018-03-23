#lang slideshow

(require slideshow/repl)

(define rep (make-repl-group))

(define test-backing
  (make-module-backing
   rep
   "#lang sexp inverse"
   "(+ 1 2)"))


(define n (make-base-namespace))
(parameterize ([current-namespace n])
  (namespace-require 'inverse)
  (namespace-require 'inverse/arithmetic)
  (namespace-require 'inverse/utils))

(slide
 #:title "How to say hello"
 (repl-area #:width (* client-w 3/4)
            #:height (* client-h 3/4)
            #:font-size 18
            #:make-namespace (thunk n)
            ""))