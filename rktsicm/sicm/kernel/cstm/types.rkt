#lang racket/base

(provide (all-defined-out))

(require (only-in "../../rkt/glue.rkt" define-integrable))

(define-integrable number-type-tag '*number*)

(define-integrable with-units-type-tag '*with-units*)

(define-integrable unit-type-tag '*unit*)

(define-integrable vector-type-tag '*vector*)

(define-integrable abstract-vector-type-tag '*vector*)

(define quaternion-type-tag '*quaternion*)


;;; Up vectors are implemented as scheme vectors

(define-integrable up-type-tag '*vector*)

(define-integrable abstract-up-type-tag '*vector*)

(define-integrable down-type-tag '*down*)

(define-integrable abstract-down-type-tag '*abstract-down*)


(define-integrable matrix-type-tag '*matrix*)

(define-integrable abstract-matrix-type-tag '*abstract-matrix*)


(define-integrable function-type-tag '*function*)

(define-integrable abstract-function-type-tag '*function*)

(define-integrable differential-type-tag '*diff*)

(define operator-type-tag '*operator*)

(define-integrable series-type-tag '*series*)


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


