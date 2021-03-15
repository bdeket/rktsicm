#lang racket/base

(provide (all-defined-out))

(require racket/fixnum
         "../kernel-intr.rkt"
         "../rkt/environment.rkt"
         "comcon.rkt"
         (only-in "../rkt/todo.rkt" todos)
         )

(todos todo
       [#:from "magic"
        compile-and-run-numerical]
       )
(require 'todo)

(define (lambda->numerical-procedure lexp)
  (compile-and-run-numerical lexp numerical-environment))

(define (generic-procedure->numerical-procedure proc [arities 1])
  (compile-and-run-numerical (lambdafy arities proc) numerical-environment))


;;; Used by find-path.

(define (lambda->interpreted-generic-procedure lexp)
  (eval lexp generic-environment))

(define (abstract-to-function vars expression)
  (lambda->interpreted-generic-procedure
   `(lambda ,vars ,expression)))

#|
(define (lambda->user-procedure lexp)
  (compile-and-run-sexp lexp user-initial-environment))


(define (lambda->generic-procedure lexp)
  (compile-and-run-sexp lexp generic-environment '()))

(define (lambda->symbolic-procedure lexp)
  (compile-and-run-sexp lexp generic-environment '()))


(define (lambda->interpreted-user-procedure lexp)
  (eval lexp user-initial-environment))

(define (lambda->interpreted-numerical-procedure lexp)
  (eval lexp numerical-environment))


(define (lambda->interpreted-symbolic-procedure lexp)
  (eval lexp generic-environment))



(define (symbolic-procedure->lambda proc)
  (let ((a (procedure-arity proc)))
    (if (equal? (car a) (cdr a))
	(s-p->l-a a proc)
	(error "Unknown arity -- symbolic-procedure->lambda" proc))))

(define (s-p->l-a arity proc)
  (let ((gens (generate-list arity
			     (lambda (i)
			       (generate-uninterned-symbol 'x)))))
    `(lambda ,gens
       ,(apply proc gens))))
|#