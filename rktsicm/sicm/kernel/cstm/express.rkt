#lang racket/base

(provide (all-defined-out))
(require "../types.rkt")

;;; An abstract quantity may be have a type-tagged expression.

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define first-operand cadr)
(define second-operand caddr)
(define rest-operands cddr)

(define up-constructor-name 'up)
(define down-constructor-name 'down)

(define (make-numerical-literal expression)
  (make-literal '*number* expression))

(define (make-real-literal expression)
  (let ((e (make-numerical-literal expression)))
    (add-property! e 'real #t)
    e))

(define (make-literal type-tag expression)
  (cons type-tag (make-hasheq (list (cons 'expression expression)))))

(define (make-combination type-tag operator operands) 
  (make-literal type-tag (cons operator operands)))

(define (expression-of abstract-quantity)
  (cond ((pair? abstract-quantity)
	 (let ((v (get-property abstract-quantity 'expression)))
	   (or v
	       (error "No expression for abstract quantity"
		      abstract-quantity))))
	((symbol? abstract-quantity)
	 abstract-quantity)
	(else
	 (error "Bad abstract quantity"))))

#|
(define (variables-in expr)
  (cond ((pair? expr)
	 (reduce list-union
		 '()
		 (map variables-in expr)))
	((symbol? expr) (list expr))
	(else '())))
|#

(define (variables-in expr)
  (let lp ((expr expr)
	   (vars '())
	   (cont (lambda (vars) vars)))
    (cond ((pair? expr)
	   (lp (car expr)
	       vars
	       (lambda (vars)
		 (lp (cdr expr)
		     vars
		     cont))))
	  ((symbol? expr)
	   (if (memq expr vars)
	       (cont vars)
	       (cont (cons expr vars))))
	  (else (cont vars)))))
