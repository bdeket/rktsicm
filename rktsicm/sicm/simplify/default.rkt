#lang racket/base

(provide (except-out (all-defined-out) assign-operation))

(require (only-in "../rkt/glue.rkt" pathname? fix:= fix:+)
         "../kernel-intr.rkt"
         "../units.rkt"
         "../rkt/undefined.rkt"
         "../rkt/environment.rkt"
         "rules.rkt"
         )
(define-values (assign-operation simplify:assign-operations)
  (make-assign-operations 'simplify))

;;bdk;; start original file

;;;; Simplifier interface
;;;   g:simplify expects typed expressions. produces untyped expressions
;;;   Needs hashtable, record, etc.


(define (default-simplify expr)
  (define (bad? expr)
    (or (boolean? expr)
	(null? expr)
	(pathname? expr)
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

#; ;;bdk;; the generic name is created in kernel/generic. because g:simplify is used in kernel.
(define g:simplify
  (make-generic-operator 1 'simplify default-simplify))
(assign-operation 'simplify default-simplify)

#|
(define (simplify-undefined expr) '*undefined-value*)
(assign-operation 'simplify simplify-undefined undefined-value?)
|#

;;; There are no simplifiers yet for compound abstract types.
;(assign-operation 'simplify expression abstract-vector?)
(assign-operation 'simplify expression abstract-up?)
(assign-operation 'simplify expression abstract-down?)
(assign-operation 'simplify expression abstract-matrix?)


;;; Series cannot be simplified except term by term.
(assign-operation 'simplify identity series?)

;;; The following simplify to themselves.
(assign-operation 'simplify identity number?)
(assign-operation 'simplify identity symbol?)
(assign-operation 'simplify identity null?)
(assign-operation 'simplify identity boolean?)
(assign-operation 'simplify identity pathname?)
(assign-operation 'simplify identity undefined-value?)


;;; Here we have notrivial simplification
#|
(define (simplify-with-units num)
  (let ((value (g:* (unit-scale (u:units num)) (u:value num)))
	(vect (unit-exponents (u:units num)))
	(system (environment-lookup scmutils-base-environment
				    (unit-system (u:units num)))))
    (make-unit-description (g:simplify value) vect system)))

(assign-operation 'simplify simplify-with-units with-units?)
|#

(define (simplify-units num)
  (let ((system (environment-lookup scmutils-base-environment
				    (unit-system (u:units num)))))
    (with-units->expression system num)))

(assign-operation 'simplify simplify-units with-units?)
(assign-operation 'simplify simplify-units units?)

;;; This must be the first handler (last in generic table) 
;;; that triggers on PROCEDURE? because it is default for 
;;; procedures.  Operators and abstract functions must
;;; be checked first.

(define (simplify-procedure expr)
  (procedure-expression expr))

(assign-operation 'simplify simplify-procedure procedure?)


(define (simplify-abstract-function expr)
  (g:simplify (f:expression expr)))

(assign-operation 'simplify simplify-abstract-function abstract-function?)


(define (simplify-operator expr)
  (g:simplify (operator-name expr)))

(assign-operation 'simplify simplify-operator operator?)


(define (simplify-quaternion expr)
  (cons 'quaternion
	(vector->list
	 ((vector-elementwise g:simplify) (cadr expr)))))

(assign-operation 'simplify simplify-quaternion quaternion?)


(define (simplify-matrix expr)
  `(matrix-by-rows
    ,@(map (lambda (r)
	     (cons 'list (vector->list r)))
	   (vector->list
	    (matrix->array ((m:elementwise g:simplify) expr))))))

(assign-operation 'simplify simplify-matrix matrix?)


(define (simplify-differential expr)
  `(make-differential-quantity
    (list ,@(map (lambda (term)
		   `(make-differential-term
		     ',(differential-tags term)
		     ,(g:simplify (differential-coefficient term))))
		 (differential-term-list expr)))))

(assign-operation 'simplify simplify-differential differential?)

(define (simplify-down expr)
  (cons down-constructor-name
	(let lp ((i 0))
	  (if (fix:= i (s:length expr))
	      '()
	      (cons (g:simplify (s:ref expr i))
		    (lp (fix:+ i 1)))))))

(assign-operation 'simplify simplify-down down?)


(define (simplify-up expr)
  (cons up-constructor-name
	(let lp ((i 0))
	  (if (fix:= i (s:length expr))
	      '()
	      (cons (g:simplify (s:ref expr i))
		    (lp (fix:+ i 1)))))))

(assign-operation 'simplify simplify-up up?)


;;; Not quite right... Should only expressionize 
;;; and simplify compound arguments to literal-function
;;; subexpressions.
(define (simplify-literal-number expr)
  (new-simplify (expression expr)))

(assign-operation 'simplify simplify-literal-number literal-number?)
