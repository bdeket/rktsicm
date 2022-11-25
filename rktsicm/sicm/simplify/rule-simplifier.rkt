#lang racket/base

(provide (all-defined-out))

(require "matcher.rkt"
         (only-in "../rkt/racket-help.rkt" warn)
         )

;;bdk;; rule-memoize is set to values in load
(define (rule-memoize x) (warn (format "rule-memoize: not memoizing - ~a" x)) x)

;;;;         Match and Substitution Language Interpreter

;;;   This is a descendent of the infamous 6.001 rule interpreter,
;;; originally written by GJS for a lecture in the faculty course held
;;; at MIT in the summer of 1983, and subsequently used and tweaked
;;; from time to time.  This subsystem has been a serious pain in the
;;; ass, because of its expressive limitations, but I have not had the
;;; guts to seriously improve it since its first appearance. -- GJS

;;; January 2006.  I have the guts now! The new matcher is based on
;;; combinators and is in matcher.scm.  -- GJS

;;; "Call-by-value", as per Alan Bundy.

(define (rule-simplifier the-rules)
  (define (simplify-expression expression)
    (if (pair? expression)
	(let ((ssubs (map simplify-expression expression)))
	  (let ((result (try-rules ssubs the-rules)))
	    (if result
		(simplify-expression result)
		ssubs)))
	expression))
  (set! simplify-expression
	(rule-memoize simplify-expression))
  simplify-expression)


(define (try-rules expression the-rules)
  (define (scan rules)
    (if (null? rules)
	#f
	(or ((car rules) expression)
	    (scan (cdr rules)))))
  (scan the-rules))


;;;;  Rule applicator, using combinator-based matcher.

(define (rule:make pattern-expression consequent)
  (let ((matcher (match:->combinators pattern-expression)))
    (define (the-rule expression)
      (matcher (list expression)
	       '()
	       (lambda (dictionary unmatched-tail)
		 (and (null? unmatched-tail)
		      (apply consequent
			     (map (lambda (binding)
				    (let ((v (match:value binding)))
				      (if (vector? v)
					  (match:extract-segment v)
					  v)))
				  dictionary))))))
    the-rule))

(define (match:extract-segment v)
  (let ((beg (match:segment-beginning v))
	(end (match:segment-end v)))
    (if (null? end) beg
	(let lp ((p beg))
	  (if (eq? p end) '()
	      (cons (car p) (lp (cdr p))))))))

#|
;;; "Call-by-name", as per Alan Bundy.
;;; Crudely written.  Does not produce same answers...sigh.

(define (rule-simplifier the-rules)
  (define (simplify-exprs exprs resimp?)
    (let ((result
	   (let lp ((exprs exprs))
	     (cond ((null? exprs) '())
		   ((try-rules (car exprs) the-rules)
		    => (lambda (result)
			 (set! resimp? #t)
			 (cons (if (pair? result)
				   (lp result)
				   result)
			       (cdr exprs))))
		   (else
		    (cons (car exprs)
			  (lp (cdr exprs))))))))
      (if resimp?
	  (simplify-expression result)
	  result)))
  (define (simplify-expression expression)
    (if (pair? expression)
	(let ((result (try-rules expression the-rules)))
	  (if result
	      (if (pair? result)
		  (simplify-exprs result #t)
		  result)
	      (simplify-exprs expression #f)))
	expression))
  (rule-memoize simplify-expression))
|#
