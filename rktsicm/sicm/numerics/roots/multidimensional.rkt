#lang racket/base

(provide (all-defined-out))

(require "../../rkt/fixnum.rkt"
         "../../kernel-intr.rkt"
         "../../general/assert.rkt"
         "../linear.rkt"
         )

;;;; Multidimensional root finder, with secant approximation to derivative.
;;;   GJS -- 10 May 2005

;;; f is a vector-valued function of a vector argument
(define (multidimensional-root f initial-point initial-step min-step)
  (multidimensional-root-internal f initial-point initial-step min-step
                                  (lambda (x) x)
                                  barf-on-zero-pivot))

(define (multidimensional-root-internal f initial-point initial-step min-step
                                        succeed fail)
  (let ((N (v:dimension initial-point)))
    (define (step xn xn-1)
      (let ((fn (f xn))
	    (fps
	     (v:generate N
	       (lambda (i)
		 (f
		  (vector-with-substituted-coord xn i
						 (vector-ref xn-1 i)))))))
	(assert (fix:= N (v:dimension fn)))
	(let ((M
	       (matrix:generate N N
				(lambda (i j)
				  (/ (- (vector-ref fn i)
					(vector-ref (vector-ref fps j) i))
				     (- (vector-ref xn j)
					(vector-ref xn-1 j)))))))
	  (lu-solve M fn
                    (lambda (x)
                      (vector-vector xn x))
                    fail))))
    (define (good-root? xn xn-1)
      (let lp ((i 0) (diff 0))
	(if (fix:= i N)
	    (< diff min-step)
	    (lp (fix:+ i 1)
		(max diff
		     (magnitude (- (vector-ref xn i)
                                   (vector-ref xn-1 i))))))))
    (define (try xn xn-1)
      (if (good-root? xn xn-1)
	  (succeed xn)
	  (try (step xn xn-1) xn)))
    (try initial-point
	 (if (vector? initial-step)
	     (v:generate N
			 (lambda (i)
			   (+ (vector-ref initial-point i)
			      (vector-ref initial-step i))))
	     (v:generate N
			 (lambda (i)
			   (+ (vector-ref initial-point i)
			      initial-step)))))))

#|
(multidimensional-root 
 (lambda (v)
   (let ((x (vector-ref v 0)) (y (vector-ref v 1)))
     (vector (+ x y -3) (+ x (- y) -1))))
 (vector 1.5 1.5)
 .01
 1e-10)
;Value: #(2. 1.)

(multidimensional-root 
 (lambda (v)
   (let ((x (vector-ref v 0)) (y (vector-ref v 1)))
     (vector (square x) (+ x (- y) -1))))
 (vector 1.5 1.5)
 .01
 1e-10)
;Value: #(1.194318926912262e-10 -.9999999998805681)

(multidimensional-root 
 (lambda (v)
   (let ((x (vector-ref v 0)) (y (vector-ref v 1)))
     (vector (+ x (- y) -1) (square (+ x y -3)))))
 (vector 1.5 1.4)
 .01
 1e-15)
;Value: #(1.9999999999999996 .9999999999999997)
|#
