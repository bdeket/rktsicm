#lang racket/base

(provide (all-defined-out)
         (all-from-out "cstm/generic-apply.rkt")
         make-assign-operations)

;;;; Primitive Generic Operation Declarations
(require "cstm/generic-apply.rkt"
         "../rkt/default-object.rkt"
         "structs.rkt"
         "numbers.rkt"
         )

;;; Unary Operators 

(define (g:transpose thing [shape default-object])
  (if (default-object? shape)
      (generic:transpose thing)
      (s:transpose1 thing shape)))

;moved to calculus/manifold
#;(assign-operation 'dimension (lambda (x) (coordinate-system-dimension x)))

;;; Binary Operators

(define (g:expt x y)
  (cond ((and (number? x) (number? y)) (n:expt x y))
	;;((g:zero? x) x) ;No! consider 0^{-1}
	((g:one? x) x)
	((g:zero? y) (g:one-like x))
	((g:one? y) x)
	(else (generic:expt x y))))

