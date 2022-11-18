#lang racket/base

(provide (all-defined-out))

(require "../rkt/fixnum.rkt"
         "../kernel-intr.rkt"
         "../units.rkt"
         "../rkt/undefined.rkt"
         "../rkt/environment.rkt"
         "rules.rkt"
         )

;;;; Simplifier interface
;;;   g:simplify expects typed expressions. produces untyped expressions
;;;   Needs hashtable, record, etc.

(define (default-simplify expr)
  (define (bad? expr)
    (or (boolean? expr)
	(null? expr)
	(path? expr)
	(undefined-value? expr)
	(and (pair? expr)
	     (not (list? expr)))))
  (cond ((list? expr)
	 (let ((subs
		(map (lambda (x) (g:simplify x))
		     expr)))
	   (if (not (rexists bad? subs))
	       (new-simplify subs)
	       subs)))
	((pair? expr)
	 (cons (g:simplify (car expr))
	       (g:simplify (cdr expr))))
	(else expr)))

;(define g:simplify (make-generic-operator 1 'simplify default-simplify))
(assign-operation generic:simplify default-simplify)

#|
(define (simplify-undefined expr) '*undefined-value*)
(assign-operation generic:simplify simplify-undefined undefined-value?)
|#

;;; There are no simplifiers yet for compound abstract types.
;(assign-operation generic:simplify expression abstract-vector?)
(assign-operation generic:simplify expression abstract-up?)
(assign-operation generic:simplify expression abstract-down?)
(assign-operation generic:simplify expression abstract-matrix?)


;;; Series cannot be simplified except term by term.
(assign-operation generic:simplify identity series?)

;;; The following simplify to themselves.
(assign-operation generic:simplify identity number?)
(assign-operation generic:simplify identity symbol?)
(assign-operation generic:simplify identity null?)
(assign-operation generic:simplify identity boolean?)
(assign-operation generic:simplify identity path?)
(assign-operation generic:simplify identity undefined-value?)


;;; Here we have notrivial simplification
#|
(define (simplify-with-units num)
  (let ((value (g:* (unit-scale (u:units num)) (u:value num)))
	(vect (unit-exponents (u:units num)))
	(system (environment-lookup scmutils-base-environment
				    (unit-system (u:units num)))))
    (make-unit-description (g:simplify value) vect system)))

(assign-operation generic:simplify simplify-with-units with-units?)
|#

(define (simplify-units num)
  (let ((system (environment-lookup scmutils-base-environment
				    (unit-system (u:units num)))))
    (with-units->expression system num)))

(assign-operation generic:simplify simplify-units with-units?)
(assign-operation generic:simplify simplify-units units?)

;;; This must be the first handler (last in generic table) 
;;; that triggers on PROCEDURE? because it is default for 
;;; procedures.  Operators and abstract functions must
;;; be checked first.

(define (simplify-procedure expr)
  (procedure-expression expr))

(assign-operation generic:simplify simplify-procedure procedure?)


(define (simplify-abstract-function expr)
  (g:simplify (f:expression expr)))

(assign-operation generic:simplify simplify-abstract-function abstract-function?)


(define (simplify-operator expr)
  (g:simplify (operator-name expr)))

(assign-operation generic:simplify simplify-operator operator?)


(define (simplify-quaternion expr)
  (cons 'quaternion
	(vector->list
	 ((vector-elementwise g:simplify) (cadr expr)))))

(assign-operation generic:simplify simplify-quaternion quaternion?)


(define (simplify-matrix expr)
  `(matrix-by-rows
    ,@(map (lambda (r)
	     (cons 'list (vector->list r)))
	   (vector->list
	    (matrix->array ((m:elementwise g:simplify) expr))))))

(assign-operation generic:simplify simplify-matrix matrix?)


(define (simplify-differential expr)
  `(make-differential-quantity
    (list ,@(map (lambda (term)
		   `(make-differential-term
		     ',(differential-tags term)
		     ,(g:simplify (differential-coefficient term))))
		 (differential-term-list expr)))))

(assign-operation generic:simplify simplify-differential differential?)

(define (simplify-down expr)
  (cons down-constructor-name
	(let lp ((i 0))
	  (if (fix:= i (s:length expr))
	      '()
	      (cons (g:simplify (s:ref expr i))
		    (lp (fix:+ i 1)))))))

(assign-operation generic:simplify simplify-down down?)


(define (simplify-up expr)
  (cons up-constructor-name
	(let lp ((i 0))
	  (if (fix:= i (s:length expr))
	      '()
	      (cons (g:simplify (s:ref expr i))
		    (lp (fix:+ i 1)))))))

(assign-operation generic:simplify simplify-up up?)


;;; Not quite right... Should only expressionize 
;;; and simplify compound arguments to literal-function
;;; subexpressions.
(define (simplify-literal-number expr)
  (new-simplify (expression expr)))

(assign-operation generic:simplify simplify-literal-number literal-number?)
