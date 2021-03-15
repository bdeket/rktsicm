#lang racket/base

(provide (all-defined-out)
         (all-from-out "pcfpf/pcf.rkt"))

(require "pcfpf/pcf.rkt"
         "../kernel-intr.rkt"
         "../rkt/default-object.rkt"
         "../general/list-utils.rkt"
         "../general/sets.rkt"
         "pcf-fpf.rkt"
         )


(define pcf:operator-table
  `((+        ,+$poly)
    (-        ,-$poly)
    (*        ,*$poly)
    (negate   ,poly:negate)
    (expt     ,poly:expt)
    (square   ,poly:square)
    (gcd      ,(lambda (x y) (poly:gcd x y)))))

(define (pcf:expression-> expr cont [less? default-object])
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

(define pcf:operators-known
  (map car pcf:operator-table))
