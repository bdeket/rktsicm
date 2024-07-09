#lang s-exp "../extapply.rkt"

(provide (all-defined-out)
         incremental-simplifier
         enable-constructor-simplifications?)

(require "../../rkt/hashtable.rkt"
         "../../general/assert.rkt"
         "../../general/memoize.rkt"
         "express.rkt"
         "../types.rkt"
         "../../parameters.rkt")

(define symbolic-operators '())

;;bdk;; insert 1
;;; Enable simplification on construction -- wastes time?
#; ;moved to parameters
(define enable-constructor-simplifications? #t)

(define (enable-constructor-simplifications doit?)
  (assert (boolean? doit?) "argument must be a boolean.")
  (clear-memoizer-tables) 
  (enable-constructor-simplifications? doit?))

;;; Disable intermediate simplification -- wastes time.
#; ;moved to parameters
(define incremental-simplifier #f)

(define symbolic-operator-table (make-eq-hash-table))

(define (make-numsymb-expression operator-symbol operands)
  (let ((operand-expressions (map numerical-expression operands)))
    (let ((v (hash-table/get symbolic-operator-table operator-symbol #f)))
      (if v
	  (let ((newexp (apply v operand-expressions)))
	    (make-literal number-type-tag
			  (if (incremental-simplifier)
			      ((incremental-simplifier) newexp)
			      newexp)))
	  (make-combination number-type-tag operator-symbol operands)))))

(define (addto-symbolic-operator-table operator procedure)
  (set! symbolic-operators (cons operator symbolic-operators))
  (hash-table/put! symbolic-operator-table operator procedure))


;;; currently disabled -- see heuristic.scm
(define heuristic-number-canonicalizer #f)

#|
;;; From general/canonicalizer.scm
(define numerical-expression-canonicalizer
  (make-expression-canonicalizer))
|#

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
;;bdk;; insert 1 end
