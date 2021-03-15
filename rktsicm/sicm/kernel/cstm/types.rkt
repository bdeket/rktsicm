#lang racket/base

(provide (all-defined-out))

(define number-type-tag '*number*)

(define with-units-type-tag '*with-units*)

(define unit-type-tag '*unit*)

(define vector-type-tag '*vector*)

(define abstract-vector-type-tag '*vector*)

(define quaternion-type-tag '*quaternion*)


;;; Up vectors are implemented as scheme vectors

(define up-type-tag '*vector*)

(define abstract-up-type-tag '*vector*)

(define down-type-tag '*down*)

(define abstract-down-type-tag '*abstract-down*)


(define matrix-type-tag '*matrix*)

(define abstract-matrix-type-tag '*abstract-matrix*)


(define function-type-tag '*function*)

(define abstract-function-type-tag '*function*)

(define differential-type-tag '*diff*)

(define operator-type-tag '*operator*)

(define series-type-tag '*series*)


(define type-tags
  (list number-type-tag
	unit-type-tag
	with-units-type-tag
	vector-type-tag
	quaternion-type-tag
	;;abstract-vector-type-tag
	;;up-type-tag
	;;abstract-up-type-tag
	down-type-tag
	abstract-down-type-tag
	matrix-type-tag
	abstract-matrix-type-tag
	function-type-tag
	differential-type-tag
	operator-type-tag
	series-type-tag))

(define compound-type-tags
  (list vector-type-tag
	;;up-type-tag
	quaternion-type-tag
	down-type-tag
	matrix-type-tag
	series-type-tag
	abstract-matrix-type-tag))

(define abstract-type-tags
  (list number-type-tag
	vector-type-tag
	;;abstract-vector-type-tag
	;;abstract-up-type-tag
	abstract-down-type-tag
	abstract-matrix-type-tag))


