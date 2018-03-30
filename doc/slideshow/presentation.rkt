#lang slideshow

(require slideshow/repl)
(require syntax/location)



(module x inverse
  (define (p x) (+ x 2))
  (provide p))

(define path (quote-module-path x))

(define ns (make-base-empty-namespace))

(parameterize ([current-namespace ns])
  (namespace-require 'inverse)
  (namespace-require path))

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

(start-at-recent-slide)

(slide
 #:title "Demo"
 (repl-area #:width (* client-w 2/3)
            #:height (* client-h 1/2)
            #:font-size 18
            #:make-namespace (thunk ns)
            "(add1 5)"))