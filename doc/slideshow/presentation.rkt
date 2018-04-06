#lang slideshow

(require slideshow/repl)
(require syntax/location)
(require slideshow/code)

(define start-future-plans #f)
(enable-click-advance! #f)

(module x inverse
  ;(define p (addn 2))
  #;(provide p))

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


(slide
 #:title "Manually invertible functions"
 (code (define add1
         (λ-create-invertible (x)
                              (+ x 1)
                              (- x 1))))
 (repl-area
  #:width (* client-w 2/3)
  #:height (* client-h 1/2)
  #:font-size 24
  #:make-namespace (thunk ns)
  "(add1 5)"))

(slide
 #:title "Automatically invertible functions"
 (repl-area
  #:width (* client-w 2/3)
  #:height (* client-h 1/2)
  #:font-size 24
  #:make-namespace (thunk ns)
  "(define add2
     (λ-auto-invertible (x)
       (add1 (add1 x))))"))

(slide
 (titlet "Demo"))

(slide
 (titlet "Future plans"))

(if start-future-plans
    (start-at-recent-slide)
    (void))

(slide
 #:title "Future Plans"
 (item "/defer forms")
 'next
 (item "Syntax location for more error messages")
 'next
 (item "Static invertibility/type checking")
 'next
 (item "Provide invertible forms of more racket functions")
 'next
 (item "Matrix library"))

 

