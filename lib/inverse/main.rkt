#lang inverse/base

(module reader syntax/module-reader
  inverse
  #:read
  read
  #:read-syntax
  read-syntax)

(require inverse/base)
(require inverse/arithmetic)
(require inverse/strings)
(require inverse/utils)
(require inverse/test-lib)
(require (subtract-in racket
                      inverse/base
                      inverse/arithmetic
                      inverse/strings
                      inverse/utils
                      inverse/test-lib))

(provide
 (all-from-out inverse/arithmetic)
 (all-from-out inverse/strings)
 (all-from-out inverse/utils)
 (all-from-out inverse/base)
 (all-from-out inverse/test-lib)
 (all-from-out racket))