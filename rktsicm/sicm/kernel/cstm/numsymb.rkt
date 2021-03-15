#lang racket/base

(provide (all-defined-out)
         incremental-simplifier
         enable-constructor-simplifications?)

(require "express.rkt"
         "../types.rkt"
         "../../parameters.rkt")

;;; currently disabled -- see heuristic.scm
(define heuristic-number-canonicalizer #f)

;;; Disable intermediate simplification -- wastes time.
(define symbolic-operator-table (make-hasheq))

(define (make-numsymb-expression operator-symbol operands)
  (let ((operand-expressions (map numerical-expression operands)))
    (let ((v (hash-ref symbolic-operator-table operator-symbol #f)))
      (if v
	  (let ((newexp (apply v operand-expressions)))
	    (make-literal number-type-tag
			  (if (incremental-simplifier)
			      ((incremental-simplifier) newexp)
			      newexp)))
	  (make-combination number-type-tag operator-symbol operands)))))

(define (addto-symbolic-operator-table operator procedure)
  (hash-set! symbolic-operator-table operator procedure))

(define numerical-expression-canonicalizer #f)

(define (numerical-expression expr)
  (cond ((number? expr)
	 (if (and (inexact? expr) heuristic-number-canonicalizer)
	     (heuristic-number-canonicalizer expr)
	     expr))
	((symbol? expr) expr)
	((literal-number? expr)
	 (if numerical-expression-canonicalizer
	     (numerical-expression-canonicalizer (expression-of expr))
	     (expression-of expr))
	 (expression-of expr))
	((pair? expr)
	 (cond ((memq (car expr) type-tags) expr)
	       (numerical-expression-canonicalizer
		(numerical-expression-canonicalizer expr))
	       (else expr)))
	(else expr)))
