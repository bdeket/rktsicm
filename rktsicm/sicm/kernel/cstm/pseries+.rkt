#lang racket/base

(provide (all-defined-out)
         (all-from-out "pseries.rkt"))

(require "../../rkt/fixnum.rkt"
         "pseries.rkt"
         "generic.rkt"
         "../strutl.rkt"
         "../types.rkt"
         "../utils.rkt")

;;; This procedure produces the result of substituting the argument
;;; for the indeterminate in the given power series.  

;;; Note, if the argument is an OPERATOR, the resulting series may be
;;; an operator too, as the series is an implicit summation.

(define (series:value series arguments)
  (define (collect stream-of-procs)
    (let ((first-result (g:apply (head stream-of-procs) arguments)))
      (if (series? first-result)
	  (let ((fr (series->stream first-result)))
	    (cons-stream (head fr)
			 (stream:+ (tail fr)
				   (collect (tail stream-of-procs)))))
	  (cons-stream first-result
		       (collect (tail stream-of-procs))))))
  (cond ((equal? (series:arity series) *exactly-one*)
	 (cond ((fix:= (length arguments) 1)
		(make-series *exactly-zero*
		 (map-streams g:*
			      (series->stream series)
			      (stream-of-powers (car arguments)
						(g:one-like
						 (car arguments))))))
	       (else
		(error "Wrong number of args to series" series arguments))))
	((equal? (series:arity series) *exactly-zero*)
	 (make-series *exactly-zero*
	  (collect (series->stream series))))
	(else
	 (error "Bad arity series" series arguments))))

