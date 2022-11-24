#lang s-exp "extapply.rkt"

(provide (all-defined-out))

(require (only-in "../rkt/glue.rkt" if)
         "../rkt/define.rkt"
         (only-in "../rkt/todo.rkt" rationalize->exact)
         "cstm/generic.rkt"
         "numeric.rkt"
         "numsymb.rkt")

;;; Some processes, such as finding the roots of a polynomial, can
;;; benefit by heuristic rounding of results (to a nearby rational).


;;; Heuristic rounding will occur to a rational within 

(define heuristic-rounding-tolerance 1.0e-13)

;;; that is expressible with a denominator less than the 

(define heuristic-rounding-denominator 20)

;;; if such a rational exists.


;;; Complex numbers with one part small enough are forced to be real
;;; or imaginary.

(define heuristic-one-part-insignificant 1.0e-10)

;;; If both real part and imaginary part of a complex number are tiny,
;;; it is also set to zero.  This is dangerous.

(define heuristic-rounding-tiny 1.0e-15)


;;; If the number is a small rational multiple of an important
;;; constant, we may substitute the symbolic constant:

(define heuristic-symbolize? #t)

(define (heuristic-canonicalize-real a #:optional symbolize? zero-threshold)
  (if (default-object? symbolize?) (set! symbolize? heuristic-symbolize?))
  (if (default-object? zero-threshold) (set! zero-threshold heuristic-rounding-tiny))
  (h-c-r a symbolize? zero-threshold))

(define (heuristic-round-real x #:optional zero-threshold)
  (if (default-object? zero-threshold)
      (h-c-r x #f heuristic-rounding-tiny)
      (h-c-r x #f zero-threshold)))

(define (h-c-r a symbolize? zero-threshold)
  (if (and zero-threshold (< (abs a) zero-threshold))
      0
      (let lp ((ideas *important-numbers*))
	(if (null? ideas)
	    a
	    (let* ((ag (/ a (caar ideas)))
		   (af (rationalize->exact ag heuristic-rounding-tolerance)))
	      (if (and (not (= af 0))
		       (< (denominator af) heuristic-rounding-denominator))
		  (if symbolize?
		      (symb:* af (cadar ideas)) ;symbolic version
		      (* af (caar ideas)))
		  (lp (cdr ideas))))))))
  
(define *important-numbers*
  `((1 1)
    (,pi :pi)
    (,(/ 1 pi) (/ 1 :pi))
    (,(exp 1) (exp 1))
    (,(exp -1) (exp -1))
    (,(sqrt 2) (sqrt 2))
    (,(sqrt 3) (sqrt 3))
    (,(sqrt 5) (sqrt 5))
    (,(sqrt 7) (sqrt 7))
    (,:euler :euler)
    (,:phi :phi)
    ))

(define (heuristic-canonicalize-complex z #:optional symbolize? zero-threshold)
  (if (default-object? symbolize?) (set! symbolize? heuristic-symbolize?))
  (if (default-object? zero-threshold) (set! zero-threshold heuristic-rounding-tiny))
  (h-c-c z symbolize? zero-threshold))

(define (heuristic-round-complex z #:optional zero-threshold)
  (if (default-object? zero-threshold)
      (h-c-c z #f heuristic-rounding-tiny)
      (h-c-c z #f zero-threshold)))

(define (h-c-c z symbolize? zero-threshold)
  (if (real? z)
      (h-c-r (real-part z) symbolize? zero-threshold) ;gets 3.5 from 3.5+0.0i
      (let ((r (real-part z)) (i (imag-part z)))
	(let ((ar (abs r)) (ai (abs i)))
	  (cond ((and (< ar zero-threshold)
		      (< ai zero-threshold))
		 0)
		((< ai (* heuristic-one-part-insignificant ar))
		 (h-c-r r symbolize? zero-threshold))
		((< ar (* heuristic-one-part-insignificant ai))
		 (g:make-rectangular 0 (h-c-r i symbolize? zero-threshold)))
		(else
		 (let* ((a (angle z))
			(af (h-c-r a symbolize? zero-threshold)))
		   (if (or (not (number? af)) (not (= af a)))
		       (g:make-polar (h-c-r (magnitude z) symbolize? zero-threshold)
				     af)
		       (g:make-rectangular (h-c-r r symbolize? zero-threshold)
					   (h-c-r i symbolize? zero-threshold))))))))))

;;; (set! heuristic-number-canonicalizer heuristic-canonicalize-complex)

#|
;;; Watch out--symb:pi is now in numsymb.scm

(define (heuristic-round-angle a)
  (let* ((ag (/ a :pi))
	 (af (heuristic-round-real ag)))
    (if (< (abs (- ag af)) heuristic-rounding-tolerance)
	(if heuristic-symbolize?
	    (g:* af symb:pi)
	    (* af :pi))
	a)))

(define symb:pi ':pi)
(define symb:-pi (symb:- 0 symb:pi))
(define symb:pi/6 (symb:/ symb:pi 6))
(define symb:-pi/6 (symb:- 0 symb:pi/6))
(define symb:pi/3 (symb:/ symb:pi 3))
(define symb:-pi/3 (symb:- 0 symb:pi/3))
(define symb:pi/2 (symb:/ symb:pi 2))
(define symb:-pi/2 (symb:- 0 symb:pi/2))
(define symb:pi/4 (symb:/ symb:pi 4))
(define symb:-pi/4 (symb:- 0 symb:pi/4))
(define symb:2pi (symb:* 2 symb:pi))
(define symb:-2pi (symb:- 0 symb:2pi))
|#
