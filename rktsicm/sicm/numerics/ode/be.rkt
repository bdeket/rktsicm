#lang racket/base

(provide (all-defined-out))

(require "../../rkt/fixnum.rkt"
         "../../kernel-intr.rkt"
         "advance.rkt"
         "qc.rkt"
         )

;;; Backwards Euler implicit integrator. 

#|
((advance-generator
  ((quality-control c-euler 1)		;integration method
   (lambda (v) v)			;x' = x
   0.0001				;qc error tolerated
   1.0e-5))				;corrector convergence
 #(1.0)					;initial state (at t = t0)
 1.0					;proceed to t = t0 + 1
 0.1					;first step no larger than .1
 0.5					;no step larger than .5
 (lambda (ns dt h cont)
   (pp ns)
   (cont))
 (lambda (ns dt sdt)
   ;; assert ns = #(2.718...)
   ;; assert dt = 1.000...+-
   (list ns dt sdt)))
|#


(define (c-euler f qc-tolerance [convergence-tolerance qc-tolerance])
  (let ((error-measure
	 (parse-error-measure convergence-tolerance)))
    (lambda (xn)
      (let ((d (f xn)))
	(define (estep dt succeed fail)
	  (let* ((predicted (vector+vector xn (scalar*vector dt d)))
		 (corrected
		  (vector+vector xn (scalar*vector dt (f predicted)))))
	    (let lp ((predicted predicted) (corrected corrected) (count 1))
	      (let ((verr (error-measure predicted corrected)))
		(if (< verr 2.0)
		    (succeed corrected count)
		    (let* ((ncorr
			    (vector+vector xn (scalar*vector dt (f corrected))))
			   (nverr (error-measure ncorr corrected)))
		      (if (< nverr verr)
			  (lp corrected ncorr (fix:+ count 1))
			  (begin (when pc-wallp? (println `(pc failed: ,nverr ,verr)))
				 (fail)))))))))
	estep))))
