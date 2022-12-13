#lang racket

(provide sqrt-expt-simplify
         sqrt-expt-simplify?)

(require (only-in "../../rkt/glue.rkt" true)
         "../../general/assert.rkt"
         "../../general/memoize.rkt")

;for numsymb

;;bdk;; insert 1
(define sqrt-expt-simplify? true)
;;bdk;; insert 1 end

;;bdk;; insert 2
(define (sqrt-expt-simplify doit?)
  (assert (boolean? doit?) "argument must be a boolean.")
  (clear-memoizer-tables)
  (set! sqrt-expt-simplify? doit?))
;;bdk;; insert 2 end
