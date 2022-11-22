#lang debug racket/base

(provide (except-out (all-defined-out) assign-operation)
         (all-from-out "cstm/pseries+.rkt"))

(require "../rkt/fixnum.rkt"
         "cstm/pseries+.rkt"
         "../general/assert.rkt"
         "cstm/generic-apply.rkt"
         "cstm/mathutil.rkt"
         "numeric.rkt"
         "strutl.rkt"
         "types.rkt"
         "utils.rkt"
         (only-in "../rkt/todo.rkt" div-coeff sub-coeff expt-coeff)
         "todo/display-print.rkt"
         )
(define-values (assign-operation pseries:assign-operations)
  (make-assign-operations 'pseries))

;;;; Power-series arithmetic using infinite streams.

(define (series:print s . optionals)
  (apply series:for-each
	 print-expression
	 s
	 optionals))

;;; The following procedure takes a finite list and makes an infinite
;;; series that has the finite list as the initial segment of the
;;; coefficients.

(define (series . args)
  (make-series *exactly-zero*
    (let lp ((a args))
      (if (null? a)
	  (infinite-stream-of (g:zero-like (car args)))
	  (stream-cons (car a) (lp (cdr a)))))))

(define (power-series . args)
  (make-series *exactly-one*
    (let lp ((a args))
      (if (null? a)
	  (infinite-stream-of (g:zero-like (car args)))
	  (stream-cons (car a) (lp (cdr a)))))))

(define series:zero 
  (make-series *exactly-one* zero-stream))

(define series:one 
  (make-series *exactly-one*
	       (stream-cons :one zero-stream)))

(define series:identity
  (make-series *exactly-one*
	       (stream-cons :zero
			    (stream-cons :one zero-stream))))


(define (constant-series c [arity *exactly-one*])
  (make-series arity
	       (stream-cons c zero-stream)))
    

;;; The following procedures provide a set of capabilities for
;;;  manipulating series.

(define (coefficient+series c series)
  (let ((s (series->stream series)))
    (make-series (series:arity series)
      (stream-cons (g:+ c (stream-first s)) (stream-rest s)))))

(define (series+coefficient series c)
  (let ((s (series->stream series)))
    (make-series (series:arity series)
      (stream-cons (g:+ (stream-first s) c) (stream-rest s)))))

(define (coefficient-series c series)
  (let ((s (series->stream series)))
    (make-series (series:arity series)
      (stream-cons (g:- c (stream-first s))
		   (negate-stream (stream-rest s))))))

(define (series-coefficient series c)
  (let ((s (series->stream series)))
    (make-series (series:arity series)
      (stream-cons (g:- (stream-first s) c) (stream-rest s)))))


;;; c*(a0 + a1*x + a2*x^2 + a3*x^3 + ...)
;;;  = c*a0 + c*a1*x + c*a2*x^2 + c*a3*x^3 + ...

(define (coefficient*series c s)
  (make-series (series:arity s)
    (map-stream (lambda (x) (g:* c x))
		(series->stream s))))

(define (series*coefficient s c)
  (make-series (series:arity s)
    (map-stream (lambda (x) (g:* x c))
		(series->stream s))))


(define (series/coefficient s c)
  (make-series (series:arity s)
    (map-stream (lambda (x) (g:/ x c))
		(series->stream s))))
    
(define (coefficient/series c s)
  (series:mul (constant-series c (series:arity s))
	      (series:invert s)))

;;; (a0 + a1*x + a2*x^2 + ...) + (b0 + b1*x + b2*x^2 + ...)
;;;   = (a0+b0) + (a1+b1)*x + (a2+b2)*x^2 + ...

(define add-series (combiner-padded-streams g:+ :zero))

(define series:add (series-wrapper add-series))

(define sub-series (combiner-padded-streams g:- :zero))

(define series:sub (series-wrapper sub-series))

(define series:negate (series-wrapper negate-stream))


;;; (a0 + a1*x + a2*x^2 + ...) * (b0 + b1*x + b2*x^2 + ...)
;;;   = a0*b0 + (a0*b1+a1*b0)*x + (a0*b2+a1*b1+a2*b0)*x^2 + ...
;;; Each coefficient of the result is formed by reversing an initial
;;;  segment of one series, multiplying it by the coefficients of an
;;;  initial segment of the other series, and accumulating the
;;;  products.

(define (stream:c*s c s)
  (map-stream (lambda (x) (g:* c x)) s))

(define (stream:s/c s c)
  (map-stream (lambda (x) (g:/ x c)) s))

(define (mul-series s1 s2)
  (stream-cons (g:* (stream-first s1) (stream-first s2))
	       (add-series (stream:c*s (stream-first s1) (stream-rest s2))
		    (mul-series (stream-rest s1) s2))))

(define series:mul (series-wrapper mul-series))

;;; what's this?
(define mul$series
  (accumulation series:mul series:one))

(define (invert-series s)
  (let ((s0 (g:/ :one (stream-first s))))
    (define inverted
      (stream-cons s0
        (mul-series (stream:c*s (g:negate s0) (stream-rest s)) inverted)))
    inverted))

(define series:invert (series-wrapper invert-series))

(define (series:div s1 s2)
  (series:mul s1 (series:invert s2)))

(define div$series
  (inverse-accumulation series:div
			series:mul
			series:invert
			series:one))  


(define (series:expt s e)
  (letrec ((square (lambda (s) (mul-series s s)))
	   (series:one
	    (stream-cons :one zero-stream))
	   (zuras
	    (lambda (t e k)
	      (stream-cons :one
		(stream:c*s (div-coeff e k)
			    (mul-series t
					(zuras t
					       (sub-coeff e 1)
					       (fix:+ k 1)))))))
	   (iexpt
	    (lambda (s e)
	      (cond ((fix:< e 0)
		     (invert-series (iexpt s (fix:- e))))
		    ((fix:= e 0) :one)
		    ((fix:= e 1) s)
		    ((even? e)
		     (square
		      (iexpt s (fix:quotient e 2))))
		    (else
		     (mul-series s
		       (square
			(iexpt s
			       (fix:quotient (fix:- e 1)
					     2))))))))
	   (expt
	    (lambda (s e)
	      (if (exact-integer? e)
		  (iexpt s e)
		  (stream:c*s (expt-coeff (stream-first s) e)
		    (zuras (stream:s/c (stream-rest s) (stream-first s)) e 1))))))
    (make-series (series:arity s)
		 (expt (series->stream s) e))))

(define series:derivative
  (let ()
    (define (deriv-iter s n)
      (if (null? s)
	  '()
	  (stream-cons (g:* n (stream-first s))
		       (deriv-iter (stream-rest s) (fix:+ n 1)))))
    (define (derivative s varnums)
      (cond ((equal? (series:arity s) *exactly-zero*)
	     ((series:elementwise
	       (lambda (term)
		 (generic:partial-derivative term varnums)))
	      s))
	    ((equal? (series:arity s) *exactly-one*)
	     (when (not (null? varnums))
		 (error "Cannot yet take partial derivatives of a series"
			s varnums))
	     (make-series *exactly-one*
			  (deriv-iter (stream-rest (series->stream s)) 1)))
	    (else
	     (error "Cannot take derivative of non arity=1 series"
		    s varnums))))
    derivative))

;;;  A series of arity zero may be summed to yield a value.
;;;  Given a stream that represents such a series, the
;;;  following procedure will produce a stream of partial sums.
;;;  Note that this sequence is a stream, not a series.

(define (partial-sums series)
  (when (not (equal? (series:arity series) *exactly-zero*))
      (error "Cannot sum non arity=0 series" series))
  (let ((stream (series->stream series)))
    (partial-sums-stream (stream-first stream) (stream-rest stream))))

(define (partial-sums-stream value s)
  (stream-cons value
	       (partial-sums-stream (g:+ value (stream-first s))
				    (stream-rest s))))

(define (series:sum series order)
  (g:ref (partial-sums series) order))


;;; This procedure produces the result of substituting the argument
;;; for the indeterminate in the given power series.  

;;; Note, if the argument is an OPERATOR, the resulting series may be
;;; an operator too, as the series is an implicit summation.

(define (series:->function series)
  (cond ((equal? (series:arity series) *exactly-zero*)
	 (series:promote-arity series))
	((equal? (series:arity series) *exactly-one*)
	 series)
	(else
	 (error "Wrong arity SERIES:->FUNCTION" series))))


;;; To go the other way we need Taylor's theorem to give us a power series:

(define (series:function-> f . opt)
  (let ((x0 (if (null? opt) :zero (car opt))))
    (make-series *exactly-one*
		 (let lp ((i 1) (fn f) (factn 1))
		   (stream-cons (g:/ (fn x0) factn)
				(lp (fix:+ 1 i)
                                    ;TODO: was (derivative fn), not sure if this is right
				    (g:derivative fn)
				    (* factn i)))))))


;;; To expand a series in a power of the argument

(define (series:zero-like x)  series:zero)
(define (series:one-like x)   series:one)


(assign-operation 'type             series:type             series?)
(assign-operation 'type-predicate   series:type-predicate   series?)
(assign-operation 'arity      series:arity                  series?)
#|
(assign-operation 'one        series:one           series?)
(assign-operation 'zero       series:zero          series?)
(assign-operation 'identity   series:identity      series?)
|#
(assign-operation 'zero-like  series:zero-like     series?)
(assign-operation 'one-like   series:one-like      series?)

(assign-operation 'negate     series:negate        series?)
(assign-operation 'invert     series:invert        series?)

(assign-operation '+          series:add           series?        series?)
(assign-operation '+          coefficient+series   not-series?    series?)
(assign-operation '+          series+coefficient   series?        not-series?)

(assign-operation '-          series:sub           series?        series?)
(assign-operation '-          coefficient-series   not-series?    series?)
(assign-operation '-          series-coefficient   series?        not-series?)

(assign-operation '*          series:mul           series?        series?)
(assign-operation '*          coefficient*series   not-series?    series?)
(assign-operation '*          series*coefficient   series?        not-series?)

(assign-operation '/          series:div           series?        series?)
(assign-operation '/          coefficient/series   not-series?    series?)
(assign-operation '/          series/coefficient   series?        not-series?)


(assign-operation 'solve-linear-right       series:div           series?        series?)
(assign-operation 'solve-linear-right       coefficient/series   not-series?    series?)
(assign-operation 'solve-linear-right       series/coefficient   series?        not-series?)

(assign-operation 'solve-linear-left
                  (lambda (x y) (series:div y x))         series?  series?)
(assign-operation 'solve-linear-left
                  (lambda (x y) (series/coefficient y x)) series?  not-series?)
(assign-operation 'solve-linear-left
                  (lambda (x y) (coefficient/series y x)) not-series?  series?)

(assign-operation 'solve-linear
                  (lambda (x y) (series:div y x))         series?     series?)
(assign-operation 'solve-linear
                  (lambda (x y) (series/coefficient y x)) series?     not-series?)
(assign-operation 'solve-linear
                  (lambda (x y) (coefficient/series y x)) not-series? series?)

(assign-operation 'expt       series:expt          series?        exact-integer?)

;(assign-operation 'exp       exp-series           operator?)

(assign-operation 'partial-derivative series:derivative    series? any?)
(assign-operation 'apply              series:value         series? any?)



#| what to do here ???

		     `(integrate ,*integrate-series)
		     `(integrate-tail ,integral-series-tail)
		     `(partial-sums ,partial-sums)
		     `(->function ,->function)
		     `(function-> ,function->)

|#


;;; The coefficients of (1+x)^a
(define (binomial-series a)
  (define (binomial-helper a n c)
    (if (g:= a 0)
	(stream-cons c zero)
	(stream-cons c
	  (binomial-helper (g:- a 1) (g:+ n 1) (g:/ (g:* c a) n)))))
  (make-series *exactly-one* (binomial-helper a 1 1)))


;;; without macros

(define tan-series
  (series:div sin-series cos-series))

(define atan-series  
  (let ()
    (define (atan-helper n s)
      (if (even? n) 
	  (stream-cons 0 (atan-helper (+ n 1) s))
	  (stream-cons (* s (/ 1 n))
		       (atan-helper (+ n 1) (- s)))))
    (make-series *exactly-one*
		 (atan-helper 0 1))))

