#lang racket/base

(provide (all-defined-out))

(require (only-in "../../rkt/glue.rkt" if write-line)
         "../../kernel-intr.rkt"
         "zeros.rkt"
         )

;;bdk;; start original file

;;; In simple cases, if one knows the function and its derivative,
;;;  Newton's method is a quick-and-dirty way to find a root.  
;;;  However, one must start close to the root to get it to converge.


(define (newton-search f&df x0 eps) 
  (define (newton-improve xn)
    (f&df xn
	  (lambda (fn dfn)
	    (- xn (/ fn dfn)))))
  (let lp ((xn x0))
    (let ((xn+1 (newton-improve xn)))
      (if (close-enuf? xn xn+1 eps)
	  (average xn xn+1)
	  (lp xn+1)))))
#|
(newton-search
 (lambda (x cont)
   (write-line x)
   (cont (cos x) (- (sin x))))
 1.0
 1e-15)
1.
1.6420926159343308
1.5706752771612507
1.5707963267954879
1.5707963267948966
;Value: 1.5707963267948966


;;; If the root is multiple, the convergence is much slower 
;;;  and much less accurate.

(newton-search
 (lambda (x cont)
   (write-line x)
   (cont (- 1.0 (sin x)) (- (cos x))))
 1
 1e-15)
1
1.2934079930260234
1.4329983666650792

;;; 28 iterations here

1.570796311871084
1.570796319310356
;Value: 1.570796319310356
|#

;;; Kahan's hack speeds up search for multiple roots, but costs
;;;  a bit for simple roots.

(define (newton-kahan-search f&df x0 x1 eps) 
  (define (kahan-trick x)
    (let ((z (round (abs x))))
      (if *kahan-wallp* (write-line `(kahan ,z)))
      z))
  (define (psi x) (f&df x /))
  (define (secant-improve xn psn xn-1 psn-1)
    (- xn 
       (* psn 
	  (kahan-trick (/ (- xn xn-1)
			  (- psn psn-1))))))
  (let lp ((xn x1) (psn (psi x1)) (xn-1 x0) (psn-1 (psi x0)))
    (if (close-enuf? xn xn-1 eps)
	(average xn xn-1)
	(let ((xn+1 (secant-improve xn psn xn-1 psn-1)))
	  (lp xn+1 (psi xn+1) xn psn)))))

(define *kahan-wallp* #f)
#|
;;; for example

(newton-kahan-search
 (lambda (x cont)
   (write-line x)
   (cont (cos x) (- (sin x))))
 1.0
 2.0
 1e-15)
1.
2.
1.5423424456397141
1.5708040082580965
1.5707963267948966
1.5707963267948966
;Value: 1.5707963267948966


;;; Kahan's method really speeds things up here, but it
;;;  doesn't make the result more accurate.

(newton-kahan-search
 (lambda (x cont)
   (write-line x)
   (cont (- 1.0 (sin x)) (- (cos x))))
 1.0
 2.0
 1e-15)
1.
2.
1.564083803078276
1.5707963519994068
1.5707963255702555
1.5707963255702555
;Value: 1.5707963255702555
|#
