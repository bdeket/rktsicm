#lang racket/base

(provide (all-defined-out))

(require (only-in "../rkt/glue.rkt" undefined-value generate-uninterned-symbol true false
                  hash-table->alist hash-table/get hash-table/put!)
         "../general/list-utils.rkt"
         (only-in "../general/memoize.rkt" hash-memoize-1arg)
         (only-in "../general/hashcons.rkt" canonical-copy)
         "../kernel-intr.rkt"
         "../parameters.rkt"
         "fpf.rkt"
         "pcf.rkt"
         "rcf.rkt"
         )

;;;;       General Recursive Simplifier Maker

;;; Given a set of operations, this procedure makes a recursive
;;;  simplifier that simplifies expressions involving these
;;;  operations, treating other combinations as atomic.

;;; To break an expression up into manipulable and nonmanipulable
;;; parts with respect to a set of algebraic operators.  This is done
;;; by the introduction of auxiliary variables.

;;; For example, the equation
;;;    I = Is (exp((V2 - V3)/Vt) - 1) ; I, V2, V3
;;; can be broken into three equations
;;;    I + Is = Is*X                  ; I, X
;;;    V2/Vt - V3/Vt = Y              ; V2, V3, Y
;;;    X = (exp Y)                    ; X, Y

;;; where X and Y are new variables.  The first two parts contain only
;;; addition, subtraction, multiplication, and division and the third
;;; is not expressible in terms of those operations.


;;; Exponential expressions with non-integer exponents must become
;;; kernels, because they cannot become polynomial exponentials.

(define *inhibit-expt-simplify* #t)

(define (make-analyzer ->expression expression-> known-operators)
  (let ((auxiliary-variable-table undefined-value)
        (reverse-table undefined-value)
        (uorder undefined-value)
        (priority undefined-value))

    ;; Default simplifier
    (define (simplify expr)
      (new-analysis)
      (simplify-expression expr))


    ;; Simplify relative to existing tables
    (define (simplify-expression expr)	
      (backsubstitute (analyze-expression expr)))


    ;; Analyze relative to existing tables
    (define (analyze-expression expr)
      (parameterize ((incremental-simplifier #f))
	(base-simplify (analyze expr))))


    ;; Set up new analysis
    (define (new-analysis)		
      (set! auxiliary-variable-table (make-hash))
      (set! reverse-table (make-weak-hasheq))
      #|
      (set! auxiliary-variable-table
	    ((weak-hash-table/constructor equal-hash-mod equal? #t)))
      (set! reverse-table (make-eq-hash-table))
      |#
      (set! uorder '())
      (set! priority '())
      'done)


    ;; Define ordering of variables
    (define (set-priority! . exprs)
      (set! priority (map add-symbol! exprs))
      priority)


    ;; Get kernel table
    (define (get-auxiliary-variable-defs)
      (map (lambda (entry)
	     (list (cdr entry) (car entry)))
	   (hash-table->alist auxiliary-variable-table)))

    ;; Implementation -----------------------

    (define (analyze expr)
      (let ((vars (sort (variables-in expr) variable<?)))
	(set! uorder
	      (append (map add-symbol! priority)
		      vars)))
      (ianalyze expr))

    (define (ianalyze expr)
      (if (and (pair? expr) (not (eq? (car expr) 'quote)))
	  (let ((sexpr (map ianalyze expr)))
	    ;; At this point all subexpressions are canonical.
	    (if (and (memq (operator sexpr) known-operators)
		     (not (and *inhibit-expt-simplify*
			       (expt? sexpr)
			       (not (exact-integer?
                                     (cadr (operands sexpr)))))))
		sexpr
		(let ((as-seen (expression-seen sexpr)))
		  (if as-seen
		      as-seen
		      (new-kernels sexpr)))))
	  expr))

    (define (new-kernels expr)
      (let ((sexpr (map base-simplify expr)))
	(let ((v (hash-table/get symbolic-operator-table
				 (operator sexpr)
				 #f)))
	  (if v
	      (let ((w (apply v (operands sexpr))))
		(if (and (pair? w) (eq? (operator w) (operator sexpr)))
		    (add-symbols! w)
		    (ianalyze w)))		      
	      (add-symbols! sexpr)))))

    (define (base-simplify expr)
      (if (and (pair? expr) (not (eq? (car expr) 'quote)))
	  (expression-> expr ->expression vless?)
	  expr))

    (define (backsubstitute expr)
      (define lp
	(lambda (expr)
	  (cond ((pair? expr) (map lp expr))
		((symbol? expr)
		 (let ((v (hash-table/get reverse-table expr #f)))
		   (if v (lp v) expr)))
		(else expr))))
      (lp expr))

    (define (add-symbols! expr)
      (let ((new (map add-symbol! expr)))
	(add-symbol! new)))

    (define (add-symbol! expr)
      (if (and (pair? expr) (not (eq? (car expr) 'quote)))
	  (let ((as-seen (expression-seen expr)))
	    (if as-seen
		as-seen
		(let ((newvar
		       (generate-uninterned-symbol "kernel")))
		  (hash-table/put! auxiliary-variable-table expr newvar)
		  (hash-table/put! reverse-table newvar expr)
		  newvar)))
	  expr))

    (define (expression-seen expr)
      (hash-table/get auxiliary-variable-table expr #f))


    (define (vless? var1 var2)
      (let ((in (memq var1 uorder)))
	(cond (in
	       (cond ((memq var2 in) true)
		     ((memq var2 uorder) false)
		     (else true)))
	      ((memq var2 uorder) false)
	      (else
	       (variable<? var1 var2)))))

    (new-analysis)

    (vector simplify
	    simplify-expression
	    new-analysis
	    set-priority!
	    analyze-expression
	    get-auxiliary-variable-defs)))


(define (default-simplifier analyzer) (vector-ref analyzer 0))

(define (expression-simplifier analyzer) (vector-ref analyzer 1))

(define (initializer analyzer) (vector-ref analyzer 2))

(define (priority-setter analyzer) (vector-ref analyzer 3))

(define (expression-analyzer analyzer) (vector-ref analyzer 4))

(define (auxiliary-variable-fetcher analyzer) (vector-ref analyzer 5))

(define fpf:analyzer
  (make-analyzer fpf:->expression fpf:expression-> fpf:operators-known))

;;(define fpf:simplify (default-simplifier fpf:analyzer))
;;(define fpf:simplify (expression-simplifier fpf:analyzer))
(define fpf:simplify
  (hash-memoize-1arg
   (compose canonical-copy
	    (expression-simplifier fpf:analyzer))))


(define pcf:analyzer
  (make-analyzer pcf:->expression pcf:expression-> pcf:operators-known))

;;(define pcf:simplify (default-simplifier pcf:analyzer))
;;(define pcf:simplify (expression-simplifier pcf:analyzer))

(define pcf:simplify
  (hash-memoize-1arg
   (compose canonical-copy
	    (expression-simplifier pcf:analyzer))))

(define rcf:analyzer
  (make-analyzer rcf:->expression rcf:expression-> rcf:operators-known))

;;(define rcf:simplify (default-simplifier rcf:analyzer))
;;(define rcf:simplify (expression-simplifier rcf:analyzer))
(define rcf:simplify
  (hash-memoize-1arg
   (compose canonical-copy
	    (expression-simplifier rcf:analyzer))))

#|
((initializer rcf:analyzer))

(pp ((expression-analyzer rcf:analyzer)
     '(- i (* Is (- (exp (/ (- v2 v3) Vt)) 1)))))
(+ (* (+ 1 (* -1 kernel17)) Is) i)

(pp ((auxiliary-variable-fetcher rcf:analyzer)))
((kernel17 (exp kernel16))
 (kernel16 (/ (+ v2 (* -1 v3)) Vt)))

(pp ((expression-analyzer rcf:analyzer)
     '(exp (/ (- v3 v2) (- Vt)))))
kernel17

(pp ((expression-simplifier rcf:analyzer)
     '(- i (* Is (- (exp (/ (- v2 v3) Vt)) 1)))))
(+ (* (+ 1 (* -1 (exp (/ (+ v2 (* -1 v3)) Vt)))) Is) i)
;Unspecified return value
|#
