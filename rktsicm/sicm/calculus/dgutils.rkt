#lang racket/base

(provide (all-defined-out))

(require "../kernel-gnrc.rkt"
         "../parameters.rkt"
         "../general/memoize.rkt"
         "../general/hashcons.rkt"
         )

;;; For symbolic expressions in operators.

(define (diffop-name form)
  (cond ((operator? form) (operator-name form))
	((literal-function? form) (f:expression form))
	(else (expression form))))


;;; The following mappers only make sense if, when there is more than
;;; one structure they are all isomorphic.

(define (s:sigma/r proc . structures)
  (s:sigma/r/l proc structures))

(define (s:sigma/r/l proc structures)
  (s:sigma/l (lambda elements
	     (if (structure? (car elements))
		 (s:sigma/r/l proc elements)
		 (apply proc elements)))
	   structures))

(define (s:sigma proc . structures)
  (s:sigma/l proc structures))

(define (s:sigma/l proc structures)
  (sigma (lambda (i)
	   (apply proc
		  (map (lambda (s) (s:ref s i))
		       structures)))
	 0
	 (- (s:length (car structures)) 1)))

#|
(define R2 (rectangular 2))
(instantiate-coordinates R2 '(x y))
(define chi-R2 (R2 '->coords))
(define chi-inverse-R2 (R2 '->point))
(define R2-basis (coordinate-system->basis R2))

(pec (s:sigma/r (lambda (e) 
		  ((e (compose (literal-function 'f (-> (UP Real Real) Real))
			       chi-R2))
		   (chi-inverse-R2 (up 'x0 'y0))))
		(basis->vector-basis R2-basis)))
#| Result:
(+ (((partial 1) f) (up x0 y0)) (((partial 0) f) (up x0 y0)))
|#
|#

;;; Sometimes we need to simplify an internal result.  

(define memoized-simplify
  (hash-memoize-1arg (compose canonical-copy g:simplify)))

(define (simplify-numerical-expression expr)
  (cond
    [(and (pair? expr) (eq? (car expr) '*number*))
     ;TODO we should not be constructing tagged literals overhere
     (define result (memoized-simplify expr))
     (define H (hash-copy (cdr expr)))
     (hash-set! H 'expression result)
     (cons '*number* H)]
    #;[(and (pair? expr) (eq? (car expr) '*number*))
     (define result (make-numerical-literal
                     (memoized-simplify expr)))
     ;(set-cdr! (cdr result) (cddr expr))
     ;result
     ]
    [else expr]))


(define (with-incremental-simplifier thunk)
  (parameterize ([incremental-simplifier g:simplify]
                 [enable-constructor-simplifications? #t])
    (clear-memoizer-tables)
    (thunk)))

#|
(pp (simplify-numerical-expression
     (/ 1 (+ (/ 1 'r1) (/ 1 'r2)))))
(*number* (expression (/ (* r1 r2) (+ r1 r2))))
|#
