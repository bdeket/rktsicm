#lang racket/base

;*r* flat types, lifted from genenv.rkt
;*r* ... because cyclic dependency with > generic > ghelper

(provide (all-defined-out))

(define generic-numerical-operators
  '(	
	zero-like
	one-like
	identity-like

	negate
	invert

	square
	cube

	sqrt

	exp
	log

	exp2
	exp10
	log2
	log10

	sin
	cos
	tan
	sec
	csc

	asin
	acos

	sinh
	cosh
	tanh
	sech
	csch

	abs

	+
	-
	*
	/

	expt
	gcd

	make-rectangular
	make-polar

	real-part
	imag-part
	magnitude
	angle

	conjugate

	atan))