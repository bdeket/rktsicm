#lang racket/base

(provide (all-defined-out))

(require "../../rkt/fixnum.rkt"
         "../../general/assert.rkt"
         "generic-extra.rkt"
         "../strutl.rkt"
         "../types.rkt"
         "../utils.rkt")

;;;; Power-series arithmetic using infinite streams.

(define (series:type ps) series-type-tag)
(define (series:type-predicate ps) series?)

(define (make-series arity stream)
  (cons series-type-tag (cons arity stream)))

(define (series:arity series) (cadr series))

(define (series:promote-arity series)
  (assert (equal? (series:arity series) *exactly-zero*))
  (make-series *exactly-one* (series->stream series)))

(define (series->stream series)
  (if (not (series? series))
      (error "Not a series" series)
      (cddr series)))

(define (series:same-arity series-list)
  (if (null? series-list)
      *at-least-zero*
      (a-reduce joint-arity
		(map series:arity series-list))))

(define ((series-wrapper stream-function) . series-list)
  (let ((arity (series:same-arity series-list)))
    (make-series arity
		 (apply stream-function
			(map series->stream
			     series-list)))))

(define (series:generate p [arity *exactly-one*])
  (make-series arity
	       (let lp ((i 0))
		 (stream-cons (p i) (lp (+ i 1))))))

(define (series:for-each proc series . optionals)
  (apply stream:for-each
	 proc
	 (series->stream series)
	 optionals))

(define ((series:elementwise proc) . series-list)
  (let ((arity (series:same-arity series-list)))
    (make-series arity
		 (apply map-streams
			proc
			(map series->stream series-list)))))

(define (series:ref series index)
  (stream-ref (series->stream series) index))

(define (series:inflate series exponent)
  (assert (and (integer? exponent) (positive? exponent) (series? series)))
  (make-series (series:arity series)
	       (stream:inflate (series->stream series)
			       (fix:- exponent 1))))

(define (negate-stream s) (map-stream g:negate s))

;;; The integral of a series
;;;           a0 + a1*x + a2*x^2 + a3*x^3 + ...
;;;  is       c + a0*x + a1*x^2/2 + a2*x^3/3 + ...
;;;  and is returned by the procedure *INTEGRATE-SERIES which
;;;  takes the "initial condition" c as a required argument. 
;;;  For technical reasons, we are unable to use *INTEGRATE-SERIES 
;;;  with mutual-recursion as in
;;;
;;;    (define cos-series (*integrate-series (series:negate sin-series) 1)) ;DOESN'T
;;;    (define sin-series (*integrate-series cos-series 0))                 ;WORK!
;;;
;;;  However, we can achieve the desired effect by postponing the
;;;  attachment of the constant term, as follows. We use the procedure
;;;  INTEGRAL-SERIES-TAIL which returns the indefinite integral
;;;  part of the integrated series, i.e., {a0 a1/2 a2/3 a3/4 ...}.
;;;  Now, the mutual-recursion above can be made to work:
;;;    (define cos-series
;;;      (make-series 1 (stream-cons 1
;;;                      (series:negate (integral-series-tail sin-series)))))
;;;    (define sin-series
;;;      (make-series 1 (stream-cons 0 (integral-series-tail cos-series))))
;;;
;;;  We have a special form, INTEGRATE-SERIES, to encapsulate this ugly mess.  Look
;;;   in the file fundamental-series.scm for examples.

(define integrate-helper
  (lambda (s n)
    (stream-cons (g:/ (stream-first s) n)
		 (integrate-helper (stream-rest s) (fix:+ n 1)))))

(define (*integrate-series series constant-term)
  (make-series (series:arity series)
	       (stream-cons constant-term
			    (integrate-helper (series->stream series)
					      1))))

(define integral-series-tail
  (lambda (series)
    (integrate-helper (series->stream series) 1)))

(define cos-series
  (make-series *exactly-one*
    (stream-cons 1
		 (negate-stream (integral-series-tail sin-series)))))

(define sin-series
  (make-series *exactly-one*
	       (stream-cons 0 (integral-series-tail cos-series))))

(define exp-series
  (make-series *exactly-one*
	       (stream-cons 1 (integral-series-tail exp-series))))

(define cosh-series
  (make-series *exactly-one*
	       (stream-cons 1 (integral-series-tail sinh-series))))

(define sinh-series
  (make-series *exactly-one*
	       (stream-cons 0 (integral-series-tail cosh-series))))

