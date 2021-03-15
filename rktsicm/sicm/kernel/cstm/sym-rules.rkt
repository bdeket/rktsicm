#lang racket

(provide sqrt-expt-simplify
         sqrt-expt-simplify?)

(require "../../general/assert.rkt"
         "../../general/memoize.rkt")

;for numsymb

(define sqrt-expt-simplify? #t)

(define (sqrt-expt-simplify doit?)
  (assert (boolean? doit?) "argument must be a boolean.")
  (clear-memoizer-tables)
  (set! sqrt-expt-simplify? doit?))
