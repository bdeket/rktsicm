#lang racket/base

(provide (all-defined-out)
         (all-from-out "pcfpf/pcf-fpf.rkt"))

(require "../rkt/fixnum.rkt"
         "pcfpf/pcf-fpf.rkt"
         "../general/resource-limit.rkt"
         "../general/list-utils.rkt"
         "../kernel/utils.rkt"
         "fpf.rkt"
         "pcfpf/pcf.rkt"
         "sparse.rkt"
         "sparse-gcd.ss"
         )

;;bdk;; start original file

;;;; This is the top level of polynomial gcd stuff.


(define (poly:gcd-dispatch u v)
  (cond ((poly:zero? u) v)
	((poly:zero? v) u)
	((poly:one? u) u)
	((poly:one? v) v)
	((base? u)
	 (if (base? v)
	     (base/gcd u v)
	     (base/gcd u (sparse-base-content (poly->sparse v)))))
	((base? v)
	 (base/gcd (sparse-base-content (poly->sparse u)) v))
	(else
	 (let ((arity (gcd-check-same-arity u v)))
	   (or (with-limited-time 1.0 ;seconds
		 (lambda () (poly/gcd-sparse u v)))
	       (with-limited-time 1.0
		 (lambda () (poly/gcd-classical u v)))
	       (with-limited-time 100.0
		 (lambda () (poly/gcd-sparse u v)))
	       (and (fix:< arity *euclid-breakpoint-arity*)
		    (with-limited-time 100.0
		      (lambda () (poly/gcd-classical u v))))
	       (poly/gcd-sparse u v)
	       (if *gcd-cut-losses*
		   (or (with-limited-time *gcd-cut-losses*
			 (lambda () (poly/gcd-classical u v)))
		       poly/one)
		   (poly/gcd-classical u v)))))))

;;bdk;; moved to pcfpf/pcf-fpf 1

(define poly:gcd poly:gcd-dispatch)

;;bdk;; moved to pcfpf/pcf-fpf 2


