#lang racket/base

(provide (all-defined-out))

(require "../../kernel-intr.rkt"
         "sigfun.rkt"
         )

(define ((hanning n) f)
  (let* ((span (sigfun:span f))
	 (tmin (sigfun:min span))
	 (tmax (sigfun:max span))
	 (p (/ :2pi (- tmax tmin)))
	 (A (sqrt 2/3)))
    (sigfun:make (lambda (t)
		   (* (expt (* A
			       (- 1
				  (cos (* p (- t tmin)))))
			    n)
		      ((sigfun:procedure f) t)))
		 span)))