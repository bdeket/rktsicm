#lang racket/base

(provide (all-defined-out)
         (all-from-out "pcfpf/pcf.rkt"))

(require (only-in "../rkt/define.rkt" define default-object?)
         "../general/list-utils.rkt"
         "../general/sets.rkt"
         "pcfpf/pcf.rkt"
         "../kernel-intr.rkt"
         "pcf-fpf.rkt"
         )


;;bdk;; start original file

;;bdk;; moved to pcfpf/pcf 1

(define (pcf:expression-> expr cont #:optional less?)
  ;; cont = (lambda (poly vars) ... )
  (let ((evars
	 (sort (list-difference (variables-in expr)
				pcf:operators-known)
		(if (default-object? less?) variable<? less?))))
    (cont ((expression-walker
	    (pair-up evars
		     (poly:new-variables (length evars))
		     pcf:operator-table))
	   expr)
	  evars)))

;;bdk;; moved to pcfpf/pcf 2

(define pcf:operator-table
  `((+        ,+$poly)
    (-        ,-$poly)
    (*        ,*$poly)
    (negate   ,poly:negate)
    (expt     ,poly:expt)
    (square   ,poly:square)
    (gcd      ,(lambda (x y) (poly:gcd x y)))))

(define pcf:operators-known
  (map car pcf:operator-table))
