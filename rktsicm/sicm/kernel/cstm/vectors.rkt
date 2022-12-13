#lang racket/base

(provide (all-defined-out))
(require (only-in "../../rkt/glue.rkt" define-integrable make-initialized-vector
                  fix:= fix:+)
         "../../general/assert.rkt"
         "generic.rkt"
         "../numeric.rkt"
         "../types.rkt")

;;bdk;; insert 1
(define (v:type v) vector-type-tag)
(define (v:type-predicate v) vector-quantity?)
;;bdk;; insert 1 end

;;bdk;; insert 2
(define-integrable v:generate make-initialized-vector)
(define-integrable vector:generate make-initialized-vector)
(define-integrable v:dimension vector-length)
;;bdk;; insert 2 end

;;bdk;; insert 3
(define (v:dot-product v1 v2)
  (assert (and (vector? v1) (vector? v2))
	  "Not vectors -- V:DOT-PRODUCT" (list v1 v2))
  (let ((n (v:dimension v1)))
    (assert (fix:= n (v:dimension v2))
	    "Not same dimension -- V:DOT-PRODUCT" (list v1 v2))
    (let lp ((i 0) (ans :zero))
      (if (fix:= i n)
	  ans
	  (lp (fix:+ i 1)
	      (g:+ ans
		   (g:* (vector-ref v1 i)
			(vector-ref v2 i))))))))
;;bdk;; insert 3 end

;;bdk;; insert 4
(define (v:cross-product v w)
  (assert (and (fix:= (vector-length v) 3)
	       (fix:= (vector-length w) 3))
	  "Cross product of non-3-dimensional vectors?"
	  (list v w))
  (let ((v0 (vector-ref v 0))
	(v1 (vector-ref v 1))
	(v2 (vector-ref v 2))
	(w0 (vector-ref w 0))
	(w1 (vector-ref w 1))
	(w2 (vector-ref w 2)))
    (vector (g:- (g:* v1 w2) (g:* v2 w1))
	    (g:- (g:* v2 w0) (g:* v0 w2))
	    (g:- (g:* v0 w1) (g:* v1 w0)))))
;;bdk;; insert 4 end

