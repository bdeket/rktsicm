#lang racket/base

(provide (all-defined-out)
         (all-from-out "generic.rkt")
         make-assign-operations)

;;;; Primitive Generic Operation Declarations
(require "../ghelper.rkt"
         "generic.rkt"
         "numeric.rkt"
         "s-operator.rkt"
         )

;;; Unary Operators 

;> from mathutil.scm
(define (g:identity x) x)
;< from mathutil.scm

(assign-operation g:identity-like (lambda (x) g:identity))

;;; Generic tests are conservative.  
;;; They will return #f unless the answer is known true.

(assign-operation g:square (lambda (x) (g:* x x)))

(define g:derivative
  (make-operator
   (lambda (f)
     (generic:partial-derivative f '()))
   'derivative))

(define (g:partial-derivative f . varspecs)
  (generic:partial-derivative f varspecs))

(define (g:partial . varspecs)
  (make-operator
   (lambda (f)
     (generic:partial-derivative f varspecs))
   `(partial ,@varspecs)))


;;; Binary Operators

(define (g:=:bin x y)
  (if (and (number? x) (number? y)) (= x y) (generic:= x y)))

(define (g:<:bin x y)
  (if (and (number? x) (number? y)) (< x y) (generic:< x y)))

(define (g:<=:bin x y)
  (if (and (number? x) (number? y)) (<= x y) (generic:<= x y)))

(define (g:>:bin x y)
  (if (and (number? x) (number? y)) (> x y) (generic:> x y)))

(define (g:>=:bin x y)
  (if (and (number? x) (number? y)) (>= x y) (generic:>= x y)))

(define (g:+:bin x y)
  (cond ((and (number? x) (number? y)) (+ x y))
	((g:zero? x) y)
	((g:zero? y) x)
	(else (generic:+ x y))))


(define (g:-:bin x y)
  (cond ((and (number? x) (number? y)) (- x y))
	((g:zero? y) x)
	((g:zero? x) (g:negate y))
	(else (generic:- x y))))


(define (g:*:bin x y)
  (cond ((and (number? x) (number? y)) (* x y))
	((exact-zero? x) (g:zero-like y))
	((exact-zero? y) (g:zero-like x))
	((g:one? x) y)
	((g:one? y) x)
	(else (generic:* x y))))

;;; In g:*:bin we test for exact (numerical) zero 
;;; because it is possible to produce a wrong-type 
;;; zero here, as follows:

;;;		  |0|             |0|
;;;	  |a b c| |0|   |0|       |0|
;;;	  |d e f| |0| = |0|, not  |0|

;;; We are less worried about the zero? below,
;;; because any invertible matrix is square.


(define (g:/:bin x y)
  (cond ((and (number? x) (number? y)) (/ x y))
	;; ((g:zero? x) (g:zero-like y))  ; Ancient bug!  No consequence.
	;; ((g:zero? x) x)
	((g:one? y) x)
	(else (generic:/ x y))))

;;; Complex Operators


;;; Weird operators

;(define (g:atan y [x default-object])
;  (if (default-object? x) (g:atan1 y) (g:atan2 y x)))
(define g:atan
  (case-lambda [(y) (g:atan1 y)]
               [(y x) (g:atan2 y x)]))

;;; *enable-literal-apply* is modulated by with-literal-apply-enabled.  
;;; This procedure is defined in extapply.scm.
;;; This feature is used explicitly in ode/interface.scm.


;;; N-ary Operator extensions

(define (g:= . args)
  (g:=:n args))

(define (g:=:n args)
  (cond ((null? args) #t)
	((null? (cdr args)) #t)
	(else
	 (let lp ((args (cddr args))
		  (larg (cadr args))
		  (ans (g:=:bin (car args) (cadr args))))
	   (if (null? args)
	       ans
	       (lp (cdr args)
		   (car args)
		   (and ans (g:=:bin larg (car args)))))))))


(define (g:< . args)
  (g:<:n args))

(define (g:<:n args)
  (cond ((null? args) #t)
	((null? (cdr args)) #t)
	(else
	 (let lp ((args (cddr args))
		  (larg (cadr args))
		  (ans (g:<:bin (car args) (cadr args))))
	   (if (null? args)
	       ans
	       (lp (cdr args)
		   (car args)
		   (and ans (g:<:bin larg (car args)))))))))

(define (g:<= . args)
  (g:<=:n args))

(define (g:<=:n args)
  (cond ((null? args) #t)
	((null? (cdr args)) #t)
	(else
	 (let lp ((args (cddr args))
		  (larg (cadr args))
		  (ans (g:<=:bin (car args) (cadr args))))
	   (if (null? args)
	       ans
	       (lp (cdr args)
		   (car args)
		   (and ans (g:<=:bin larg (car args)))))))))


(define (g:> . args)
  (g:>:n args))

(define (g:>:n args)
  (cond ((null? args) #t)
	((null? (cdr args)) #t)
	(else
	 (let lp ((args (cddr args))
		  (larg (cadr args))
		  (ans (g:>:bin (car args) (cadr args))))
	   (if (null? args)
	       ans
	       (lp (cdr args)
		   (car args)
		   (and ans (g:>:bin larg (car args)))))))))


(define (g:>= . args)
  (g:>=:n args))

(define (g:>=:n args)
  (cond ((null? args) #t)
	((null? (cdr args)) #t)
	(else
	 (let lp ((args (cddr args))
		  (larg (cadr args))
		  (ans (g:>=:bin (car args) (cadr args))))
	   (if (null? args)
	       ans
	       (lp (cdr args)
		   (car args)
		   (and ans (g:>=:bin larg (car args)))))))))


(define (g:+ . args)
  (g:+:n args))

(define (g:+:n args)
  (cond ((null? args) :zero)
	((null? (cdr args)) (car args))
	(else
	 (let lp ((args (cddr args))
		  (ans (g:+:bin (car args) (cadr args))))
	   (if (null? args)
	       ans
	       (lp (cdr args)
		   (g:+:bin ans (car args))))))))

(define (g:* . args)
  (g:*:n args))

(define (g:*:n args)
  (cond ((null? args) :one)
	((null? (cdr args)) (car args))
	(else
	 (let lp ((args (cddr args))
		  (ans (g:*:bin (car args) (cadr args))))
	   (if (null? args)
	       ans
	       (lp (cdr args)
		   (g:*:bin ans (car args))))))))

(define (g:- . args)
  (g:-:n args))

(define (g:-:n args)
  (cond ((null? args) :zero)
	((null? (cdr args)) (g:negate (car args)))
	(else
	 (g:-:bin (car args)
		  (g:+:n (cdr args))))))

(define (g:/ . args)
  (g:/:n args))

(define (g:/:n args)
  (cond ((null? args) :one)
	((null? (cdr args)) (g:invert (car args)))
	(else
	 (g:/:bin (car args)
		  (g:*:n (cdr args))))))


(define (g:gcd . args)
  (g:gcd:n args))

(define (g:gcd:n args)
  (cond ((null? args) :zero)
	((null? (cdr args)) (car args))
	(else
	 (let lp
	     ((as (cddr args))
	      (ans (generic:gcd (car args) (cadr args))))
	   (cond ((null? as) ans)
		 ((g:one? ans) ans)
		 (else
		  (lp (cdr as) (generic:gcd ans (car as)))))))))

