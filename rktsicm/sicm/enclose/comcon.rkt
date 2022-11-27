#lang racket/base

(provide (all-defined-out))

(require (only-in "../rkt/glue.rkt" generate-uninterned-symbol fix:= fix:1+)
         "../kernel-intr.rkt")

;;;;                    COMCON.SCM

;;; Useful utilities for programs that construct SCHEME programs.
;;; Needs UTILS.SCM

(define (lambdafy n body-generator)
  (cond ((exact-integer? n)
	 (let ((bvl (make-bound-variables n)))
	   `(lambda ,bvl ,(expression (g:apply body-generator bvl)))))

	((list? n)
	 (let llp ((n n) (body-generator body-generator))
	   (if (null? (cdr n))
	       (let ((bvl (make-bound-variables (car n))))
		 `(lambda ,bvl ,(expression (g:apply body-generator bvl))))
	       (let ((bvl (make-bound-variables (car n))))
		 `(lambda ,bvl
		    ,(llp (cdr n) (g:apply body-generator bvl)))))))
	((and (pair? n)
	      (exact-integer? (car n))
	      (exact-integer? (cdr n))
	      (fix:= (car n) (cdr n)))
	 (lambdafy (car n) body-generator))
	((pair? n)
	 ;; In Scheme 7.5 #f=() so (3) and (3 . #f) are not distinguished.
	 (error "General arity is unimplemented -- LAMBDAFY"
		n))
	(else
	 (error "Bad variable specification -- LAMBDAFY"
		n))))

(define (make-bound-variables n)
  ;;n is a general arity
  (let do-loop ((i 0) (names '()))
    (if (fix:= i n)
        names
        (do-loop (fix:1+ i)
                 (cons (generate-uninterned-symbol 'x) names)))))





(define (letify vals body-generator)
  (if (null? vals)
      (body-generator '())
      (let ((names (map (lambda (x) (generate-uninterned-symbol 'y)) vals)))
        `(let ,(map list names vals) ,(body-generator names)))))

(define (definify name definition-expression)
  (if (pair? definition-expression)
      (if (eq? (car definition-expression) 'lambda)
	  `(define (,name . ,(cadr definition-expression))
	     . ,(cddr definition-expression))
	  `(define ,name ,definition-expression))
      `(define ,name ,definition-expression)))
	     


