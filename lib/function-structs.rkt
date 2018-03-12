#lang racket

; A Function is one of:
(struct invfunc-wrap (func invfunc))
(struct ninvfunc-wrap (func))