#lang racket/base

(provide (all-defined-out)
         (all-from-out "cstm/generic.rkt"))

;;;; Primitive Generic Operation Declarations
(require "cstm/generic.rkt"
         (only-in "../rkt/define.rkt" define default-object?)
         "structs.rkt"
         "numbers.rkt"
         )

;;bdk;; start original file

;;;; Primitive Generic Operation Declarations 


;;; Unary Operators 

;;bdk;; moved to cstm/generic 1
(define (g:transpose thing #:optional shape)
  (if (default-object? shape)
      (generic:transpose thing)
      (s:transpose1 thing shape)))

;;bdk;; moved to cstm/generic 2

;;; Binary Operators

;;bdk;; moved to cstm/generic 3

(define (g:expt x y)
  (cond ((and (number? x) (number? y)) (n:expt x y))
	;;((g:zero? x) x) ;No! consider 0^{-1}
	((g:one? x) x)
	((g:zero? y) (g:one-like x))
	((g:one? y) x)
	(else (generic:expt x y))))


;;bdk;; moved to cstm/generic 4

