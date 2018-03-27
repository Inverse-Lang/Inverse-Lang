#lang inverse/base

(module reader syntax/module-reader
  inverse
  #:read
  read
  #:read-syntax
  read-syntax)

(require (prefix-in racket))
(require inverse/base)
(require (for-syntax syntax/parse))
(require inverse/arithmetic)
(require inverse/strings)
(require inverse/utils)

(provide
 (all-from-out inverse/arithmetic)
 (all-from-out inverse/strings)
 (all-from-out inverse/utils)
 (all-from-out inverse/base)
 (except-out (all-from-out racket)
                     un:#%app))