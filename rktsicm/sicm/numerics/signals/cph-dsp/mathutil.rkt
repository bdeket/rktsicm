#lang racket/base

(provide (all-defined-out))

(require (only-in "../../../rkt/glue.rkt" if error:wrong-type-argument
                  fix:= fix:< fix:+ fix:* fix:quotient
                  flo:flonum? flo:* flo:+ flo:-)
         "flovec.rkt" (only-in (submod "flovec.rkt" flo:vector) flo:vector-cons))

;;;; Math Utilities for DSP


(define 20log10
  (let ((scale (/ 20 (log 10))))
    (lambda (x)
      (* scale (log x)))))

(define 10log10
  (let ((scale (/ 10 (log 10))))
    (lambda (x)
      (* scale (log x)))))

(define log10
  (let ((scale (/ 1 (log 10))))
    (lambda (x)
      (* scale (log x)))))

(define (passband-ripple->delta ripple)
  (- 1. (expt 10. (/ (- ripple) 40.))))

(define (passband-delta->ripple delta)
  (* -2. (20log10 (- 1. delta))))

(define (stopband-attenuation->delta attenuation)
  (expt 10. (/ (- attenuation) 20.)))

(define (stopband-delta->attenuation delta)
  (- (20log10 delta)))

(define (hz->radians f sampling-rate)
  (/ (* 2pi f) sampling-rate))

(define pi
  (* 4 (atan 1 1)))

(define 2pi
  (* 2 pi))

(define (square x)
  (* x x))

(define (vector-elementwise-product v1 v2)
  (if (not (vector? v1))
      (error:wrong-type-argument v1 "vector" 'VECTOR-ELEMENTWISE-PRODUCT))
  (if (not (vector? v2))
      (error:wrong-type-argument v2 "vector" 'VECTOR-ELEMENTWISE-PRODUCT))
  (let ((n1 (vector-length v1))
	(n2 (vector-length v1)))
    (let ((n (if (fix:< n1 n2) n1 n2)))
      (let ((result (make-vector n)))
	(do ((i 0 (fix:+ i 1)))
	    ((fix:= i n))
	  (vector-set! result i (* (vector-ref v1 i) (vector-ref v2 i))))
	result))))

(define (flo:vector-elementwise-product v1 v2)
  (if (not (flo:flonum? v1))
      (error:wrong-type-argument v1 "flonum vector"
				 'FLO:VECTOR-ELEMENTWISE-PRODUCT))
  (if (not (flo:flonum? v2))
      (error:wrong-type-argument v2 "flonum vector"
				 'FLO:VECTOR-ELEMENTWISE-PRODUCT))
  (let ((n1 (flo:vector-length v1))
	(n2 (flo:vector-length v1)))
    (let ((n (if (fix:< n1 n2) n1 n2)))
      (let ((result (flo:vector-cons n)))
	(do ((i 0 (fix:+ i 1)))
	    ((fix:= i n))
	  (flo:vector-set! result
			   i
			   (flo:* (flo:vector-ref v1 i)
				  (flo:vector-ref v2 i))))
	result))))

(define (flo:vector-elementwise-sum v1 v2)
  (if (not (flo:flonum? v1))
      (error:wrong-type-argument v1 "flonum vector"
				 'FLO:VECTOR-ELEMENTWISE-SUM))
  (if (not (flo:flonum? v2))
      (error:wrong-type-argument v2 "flonum vector"
				 'FLO:VECTOR-ELEMENTWISE-SUM))
  (let ((n1 (flo:vector-length v1))
	(n2 (flo:vector-length v1)))
    (let ((n (if (fix:< n1 n2) n1 n2)))
      (let ((result (flo:vector-cons n)))
	(do ((i 0 (fix:+ i 1)))
	    ((fix:= i n))
	  (flo:vector-set! result
			   i
			   (flo:+ (flo:vector-ref v1 i)
				  (flo:vector-ref v2 i))))
	result))))

(define (flo:vector-elementwise-difference v1 v2)
  (if (not (flo:flonum? v1))
      (error:wrong-type-argument v1 "flonum vector"
				 'FLO:VECTOR-ELEMENTWISE-DIFFERENCE))
  (if (not (flo:flonum? v2))
      (error:wrong-type-argument v2 "flonum vector"
				 'FLO:VECTOR-ELEMENTWISE-DIFFERENCE))
  (let ((n1 (flo:vector-length v1))
	(n2 (flo:vector-length v1)))
    (let ((n (if (fix:< n1 n2) n1 n2)))
      (let ((result (flo:vector-cons n)))
	(do ((i 0 (fix:+ i 1)))
	    ((fix:= i n))
	  (flo:vector-set! result
			   i
			   (flo:- (flo:vector-ref v1 i)
				  (flo:vector-ref v2 i))))
	result))))

(define (flo:oversample samples n)
  (let ((l (flo:vector-length samples)))
    (let ((v (flo:make-vector (fix:* l n))))
      (let loop ((t 0) (t* 0))
	(if (not (fix:= t l))
	    (let ((x (flo:vector-ref samples t))
		  (next-t* (fix:+ t* n)))
	      (do ((t* t* (fix:+ t* 1)))
		  ((fix:= t* next-t*))
		(flo:vector-set! v t* x))
	      (loop (fix:+ t 1) next-t*))))
      v)))

(define (flo:decimate samples n)
  (let ((l (flo:vector-length samples))
	(n/2 (fix:quotient n 2)))
    (let ((l* (fix:quotient l n)))
      (let ((v (flo:make-vector l*)))
	(do ((t n/2 (fix:+ t n))
	     (t* 0 (fix:+ t* 1)))
	    ((fix:= t* l*))
	  (flo:vector-set! v t* (flo:vector-ref samples t)))
	v))))