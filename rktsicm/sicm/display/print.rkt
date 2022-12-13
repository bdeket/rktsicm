#lang racket/base

(provide (all-defined-out))

(require (only-in "../rkt/glue.rkt" if undefined-value? hash-table? pathname? any)
         (only-in "../rkt/todo.rkt" pp)
         (only-in "../rkt/environment.rkt" generic-environment rule-environment numerical-environment scmutils-base-environment system-global-environment)
         "../general/assert.rkt"
         "../general/memoize.rkt"
         "../kernel-intr.rkt"
         "../kernel/ghelper.rkt"
         "../kernel/todo/display-print.rkt"
         (only-in "../simplify/rules.rkt" clean-differentials)
         (only-in "../simplify/split-poly.rkt" poly:factor)
         "exdisplay.rkt"
         "suppress-args.rkt"
         (only-in "../rkt/todo.rkt" todos)
         )

(todos todo
       [#:from "???"
        with-si-units->expression]
       )
(require 'todo)

;;bdk;; start original file

;;; Hamiltonians look better if we divide them out.

(define *divide-out-terms* #f)

(define *heuristic-numbers* #f)

(define canonicalize-numbers (make-generic-operator 1 'canonicalize-numbers values))
(assign-operation canonicalize-numbers (λ (expr) (and (number? expr) *heuristic-numbers*)) heuristic-canonicalize-complex)
(assign-operation canonicalize-numbers list? (λ (expr) (cons (canonicalize-numbers (operator expr))
                                                             (map canonicalize-numbers (operands expr)))))
#; ;;bdk;; do in units
(assign-operation canonicalize-numbers with-units? with-si-units->expression)
#;
(define (canonicalize-numbers expr)
  (cond ((with-units? expr)
	 (with-si-units->expression expr))
	((list? expr)
	 (cons (canonicalize-numbers (operator expr))
	       (map canonicalize-numbers (operands expr))))
	((and (number? expr) *heuristic-numbers*)
	 (heuristic-canonicalize-complex expr))
	(else
	 expr)))

(define (ham:simplify hexp)
  (cond ((and (quotient? hexp) *divide-out-terms*)
	 (cond ((sum? (symb:numerator hexp))
		(let ((d (symb:denominator hexp)))
		  (a-reduce symb:+
			    (map (lambda (n)
				   (g:simplify (symb:/ n d)))
				 (operands (symb:numerator hexp))))))
	       (else hexp)))
	((compound-data-constructor? hexp)
	 (cons (operator hexp) (map ham:simplify (operands hexp))))
	(else hexp)))

(define (divide-out-terms-simplify doit?)
  (assert (boolean? doit?) "argument must be a boolean.")
  (clear-memoizer-tables)
  (set! *divide-out-terms* doit?))


;;; Equations are often prettier if we get rid of the denominators,
;;; but watch out for singularities.

(define (eqn:simplify hexp)
  (cond ((quotient? hexp)
	 (symb:numerator hexp))
	((matrix? hexp)
	 ((m:elementwise eqn:simplify) hexp))
	((vector? hexp)
	 ((v:elementwise eqn:simplify) hexp))
	(else hexp)))

(define (flush-derivative expr)
  (substitute derivative-symbol
	      'derivative
	      expr))

(define (flush-literal-function-constructors expr)
  (if (pair? expr)
      (if (eq? (car expr) 'literal-function)
	  (if (and (pair? (cadr expr)) (eq? (caadr expr) 'quote))
	      (flush-literal-function-constructors (cadadr expr))
	      (cadr expr))
	  (cons (flush-literal-function-constructors (car expr))
		(flush-literal-function-constructors (cdr expr))))
      expr))


(define *factoring* #f)

#|
(define (simplify exp)
  (flush-derivative
       (flush-literal-function-constructors
	(ham:simplify
	 ((if *factoring* poly:factor (lambda (expr) expr))
	  (g:simplify exp))))))
|#

(define (simplify exp)
  (clean-differentials
   (flush-derivative
    (flush-literal-function-constructors
     (ham:simplify
      ((if *factoring* poly:factor (lambda (expr) expr))
       (g:simplify exp)))))))

;;; Is this enough?
(define (careful-simplify e)
  (simplify e))

(define *only-printing* #f)
(define *last-expression-printed* (lambda () 'none-yet))

(define (system-environments)
  (list generic-environment rule-environment
        numerical-environment scmutils-base-environment))

(define (prepare-for-printing expr simplifier)
  (set! *last-expression-printed* 
	(cond ((unsimplifiable? expr)
	       (lambda () expr))
	      ((and (not (with-units? expr))
		    (apply object-name expr (system-environments)))
	       => (lambda (name) (lambda () name)))
	      (else
	       (let ((rexpr (simplifier expr)))
		  (lambda () (arg-suppressor rexpr))))))
  *last-expression-printed*)

(define (unsimplifiable? expr)
  (or (memq expr '(#t #f))
      (null? expr)
      (number? expr)
      (pathname? expr)
      (hash-table? expr)
      (undefined-value? expr)
      (and (procedure? expr)
	   (object-name expr system-global-environment))
      (improper-expression? expr)))

(define (improper-expression? expr)
  (and (pair? expr)
       (not (eq? (car expr) '*matrix*))
       (or (memq (car expr) '(*operator* *solution*)) ;What is this?
	   (not (list? expr))
	   (any improper-expression? expr))))

(define (show-expression expr [simplifier simplify])
  (prepare-for-printing expr simplifier)
  ;; (display "#;\n")
  (pp (*last-expression-printed*))
  (cond ((not *only-printing*)
	 (internal-show-expression
	  (*last-expression-printed*)))))

(define (print-expression expr [simplifier simplify])
  (prepare-for-printing expr simplifier)
  ;; (display "#;\n")
  (pp (*last-expression-printed*)))

(define pe print-expression)
(define se show-expression)

#;#; ;;bdk;; not interested
(define (print-expression-prefix expr [simplifier simplify])
  (prepare-for-printing expr simplifier)
  ((pp-line-prefix "; ") (*last-expression-printed*)))

(define pep print-expression-prefix)

(define (print-expression-comment expr [simplifier simplify])
  (prepare-for-printing expr simplifier)
  (newline)
  (display "#| Result:")
  (newline)
  (pp (*last-expression-printed*))
  (display "|#"))

(define pec print-expression-comment)

;overwriting the fct's in kernel/todo/print-simplify
(set-simplify! simplify);used in kernel/numbers
(set-print-expression! print-expression);used in pseries
(set-careful-simplify! careful-simplify);used in quaternions
