#lang racket/base

(require racket/fixnum
         (only-in racket/base [object-name procedure-name]))
(provide (all-defined-out)
         (all-from-out "cstm/express.rkt")
         procedure-name)

(require racket/port
         "cstm/express.rkt"
         "cstm/s-operator.rkt"
         "../general/eq-properties.rkt"
         "../general/list-utils.rkt"
         "../general/table.rkt"
         "../rkt/undefined.rkt"
         (only-in "../rkt/environment.rkt" user-generic-environment generic-environment rule-environment numerical-environment scmutils-base-environment)
         "cstm/diff.rkt"
         "iterat.rkt"
         "matrices.rkt"
         "cstm/numsymb.rkt"
         "cstm/structs.rkt"
         "types.rkt"
         "utils.rkt"
         (only-in "../rkt/todo.rkt" todos)
         )

(todos todo
       [#:from "???"
        with-si-units->expression])
(require 'todo)

;;;;  Utilities for manipulating symbolic expressions

(define (substitute new old expression)
  (define (sloop exp)
    (cond
      [(equal? old exp) new]
      [(pair? exp)
       (cons (sloop (car exp))
             (sloop (cdr exp)))]
      [(vector? exp)
       ((vector-elementwise sloop) exp)]
      [else exp]))
  (if (equal? new old) expression (sloop expression)))


;;; In this system, expressions never contain vectors or matrices,
;;; they only contain constructions for them.  Thus we need to be able
;;; to recognize the constructors:

(define (down-maker? expr)
  (and (pair? expr)
       (eq? (car expr) down-constructor-name)))

(define (up-maker? expr)
  (and (pair? expr)
       (eq? (car expr) up-constructor-name)))

(define (vector-maker? expr)
  (and (pair? expr)
       (eq? (car expr) 'vector)))

(define (quaternion-maker? expr)
  (and (pair? expr)
       (eq? (car expr) 'quaternion)))

(define (matrix-by-rows-maker? expr)
  (and (pair? expr)
       (eq? (car expr) 'matrix-by-rows)))

(define (matrix-by-columns-maker? expr)
  (and (pair? expr)
       (eq? (car expr) 'matrix-by-cols)))


(define (matrix-maker? expr)
  (and (pair? expr)
       (or (eq? (car expr) 'matrix-by-rows)
	   (eq? (car expr) 'matrix-by-cols))))

(define (compound-data-constructor? expr)
  (and (pair? expr)
       (memq (car expr)
	     '(list
	       vector
	       quaternion
	       down
	       up
	       matrix-by-rows
	       matrix-by-cols))))

(define (expression expr)
  (define (exprlp expr)
    (cond ((number? expr)
	   (if (and (inexact? expr) heuristic-number-canonicalizer)
	       (heuristic-number-canonicalizer expr)
	       expr))
	  ((symbol? expr) expr)	   
	  ((null? expr) expr)
	  ((differential? expr)
	   `(make-differential-quantity
	     (list ,@(map (lambda (term)
			    `(make-differential-term
			      ',(differential-tags term)
			      ,(exprlp (differential-coefficient term))))
			  (differential-term-list expr)))))
	  ((down? expr)
	   (cons down-constructor-name
		 (let lp ((i 0))
		   (if (fx= i (s:length expr))
		       '()
		       (cons (exprlp (s:ref expr i))
			     (lp (fx+ i 1)))))))
	  ((up? expr)		;subsumes vector? below.
	   (cons up-constructor-name
		 (let lp ((i 0))
		   (if (fx= i (s:length expr))
		       '()
		       (cons (exprlp (s:ref expr i))
			     (lp (fx+ i 1)))))))
#|	  
	  ((vector? expr)
	   (cons 'vector
		 (vector->list
		  ((vector-elementwise exprlp) expr))))
|#
	  ((quaternion? expr)
	   (cons 'quaternion
		 (vector->list
		  ((vector-elementwise exprlp) (cadr expr)))))
	  ((matrix? expr)
	   `(matrix-by-rows
	     ,@(map (lambda (r)
		      (cons 'list (vector->list r)))
		    (vector->list
		     (matrix->array ((m:elementwise exprlp) expr))))))
	  ((literal-number? expr)
	   (exprlp (expression-of expr)))
	  ((or (with-units? expr) (units? expr))
	   (exprlp (with-si-units->expression expr)))
	  ((pair? expr)
	   (cond ((eq? (car expr) '???) expr)
		 ((memq (car expr) abstract-type-tags)
		  (exprlp (expression-of expr)))
		 (else (safe-map exprlp expr))))
	  ((abstract-function? expr)
	   (exprlp (f:expression expr)))
	  ((operator? expr)
	   (exprlp (operator-name expr)))
	  ((procedure? expr)
	   (procedure-expression expr))
	  ((undefined-value? expr)
	   '*undefined-value*)
	  ((boolean? expr)
	   (if expr 'true 'false))
	  (else (error "Bad expression" expr))))
  (exprlp expr))

;;; Finds a name, if any, of the given object in the given
;;; environments.  If none, value is #f.

(define (object-name object . environments)
  (for*/first ([e (in-list environments)]
               [b (in-list (namespace-mapped-symbols e))]
               [o (in-value
                   (with-handlers ([exn:fail? (λ (x) (gensym))])
                     (namespace-variable-value b #t #f e)))]
               #:when (eq? object o))
    b))

(define (procedure-expression f)
  (or (eq-get f 'function-name)
      (object-name f
		   user-generic-environment
		   generic-environment
		   rule-environment
		   numerical-environment
		   scmutils-base-environment)
      (procedure-name f)
      '???))


(define (generate-list-of-symbols base-symbol n)
  (generate-list n
    (lambda (i)
      (concatenate-names base-symbol
			 (string->symbol (number->string i))))))


(define (pair-up vars vals table)
  (cond ((null? vars)
	 (cond ((null? vals) table)
	       (else
		(error "Too many vals -- PAIR-UP"
		       vars vals))))
	((null? vals)
	 (error "Too few vals -- PAIR-UP"
		vars vals))
	(else
	 (cons (list (car vars) (car vals))
	       (pair-up (cdr vars) (cdr vals)
			table)))))
		

;;; An evaluator for simple expressions

(define (expression-walker environment)
  (define (walk expr)
    (cond ((number? expr) expr)
	  ((symbol? expr)
	   (lookup expr environment))
	  ((pair? expr)
	   (apply (walk (car expr))
		  (map walk (cdr expr))))
	  (else
	   (error "Unknown expression type -- EXPRESSION-WALK"
		  expr))))
  walk)

(define (expr:< expr1 expr2)
  (cond ((null? expr1)
	 (if (null? expr2)
             #f
             #t))
	((null? expr2) #f)
	((real? expr1)
	 (if (real? expr2)
             (< expr1 expr2)
             (or (symbol? expr2) (string? expr2) (pair? expr2) (vector? expr2))))
	((real? expr2) #f)
	((symbol? expr1)
	 (if (symbol? expr2)
	     (symbol<? expr1 expr2)
	     (or (string? expr2) (pair? expr2) (vector? expr2))))
	((symbol? expr2) #f)
        ((string? expr1)
	 (if (string expr2)
	     (string<? expr1 expr2)
	     (or (pair? expr2) (vector? expr2))))
	((string? expr2) #f)
	((pair? expr1)
	 (if (pair? expr2)
             (let ((n1 (length expr1)) (n2 (length expr2)))
               (cond ((fx< n1 n2) #t)
                     ((fx< n2 n1) #f)
                     ((expr:< (car expr1) (car expr2)) #t)
                     ((expr:< (car expr2) (car expr1)) #f)
                     (else (expr:< (cdr expr1) (cdr expr2)))))
             (vector? expr2)))
	((pair? expr2) #f)
	((vector? expr1)
	 (cond ((vector? expr2)
		(cond ((fx< (vector-length expr1)
			      (vector-length expr2))
		       #t)
		      ((fx= (vector-length expr1)
			      (vector-length expr2))
		       (let ((n (vector-length expr1)))
			 (let lp ((i 0))
			   (cond ((fx= i n) #f)
				 ((expr:< (vector-ref expr1 i)
					  (vector-ref expr2 i))
				  #t)
				 ((equal? (vector-ref expr1 i)
					  (vector-ref expr2 i))
				  (lp (fx+ i 1)))
				 (else #f)))))
		      (else #f)))
	       (else #f)))
	((vector? expr2) #f)
        ;;This one is pretty weird...
	(else
	 (< (hash expr1) (hash expr2)))))

(define expr:= equal?)
