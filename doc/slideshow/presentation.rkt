#lang slideshow

(require slideshow/repl)

(define rep (make-repl-group))

(define n (make-base-namespace))
(parameterize ([current-namespace n])
  (namespace-require 'inverse)
  (namespace-require 'inverse/arithmetic)
  (namespace-require 'inverse/utils))

(module test inverse
  (require inverse/arithmetic)
  (require inverse/utils))


(slide
 (titlet "The Inverse Language")
 (t "Daniel Melcer and Josh Goldman"))

(slide
 #:title "What is TIL?"
 (item "TIL is...")
 'next
 (item "A language for definining and using invertible functions")
 'next
 (item (string-append "A way to automatically construct new invertible functions from "
                      "compositions of other invertible functions"))
 'next
 (item "A library of pre-defined invertible functions and utilities for working with such functions"))
      
(slide
 #:title "How to say hello"
 (repl-area #:width (* client-w 2/3)
            #:height (* client-h 1/2)
            #:font-size 18
            #:make-namespace (thunk n)
            "(add1 5)"))