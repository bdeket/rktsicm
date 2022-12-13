#lang racket/base

(provide (all-defined-out)
         (all-from-out "cstm/mathutil.rkt")
         g:identity)

(require (only-in "../rkt/define.rkt" define default-object?)
         "../general/list-utils.rkt"
         "numeric.rkt"
         "utils.rkt"
         "generic.rkt"
         "cstm/mathutil.rkt"
         "cstm/matrices.rkt"
         "structs.rkt"
         "strutl.rkt"
         "types.rkt"
         )


;;bdk;; start original file

;;;;    Derived Generic Operators

(define ratnum? rational?);not sure ... should this be exact?
#;
(define ratnum?
  (access ratnum?
          (->environment '(runtime number))))

(define (g:cube x)
  (g:* x x x))


(define (g:log10 x)
  (g:/ (g:log x) (g:log 10)))
(define (g:log2 x)
  (g:/ (g:log x) (g:log 2)))

(define (g:exp10 x)
  (g:expt 10 x))
(define (g:exp2 x)
  (g:expt 2 x))


;;; See numbers.scm

(define (g:tan x)
  (g:/ (g:sin x) (g:cos x)))
(define (g:cot x)
  (g:/ (g:cos x) (g:sin x)))

(define (g:sec x)
  (g:/ :one (g:cos x)))
(define (g:csc x)
  (g:/ :one (g:sin x)))


(define (g:tanh x)
  (g:/ (g:sinh x) (g:cosh x)))
(define (g:sech x)
  (g:/ :one (g:cosh x)))
(define (g:csch x)
  (g:/ :one (g:sinh x)))

(define (g:asinh z)
  (g:log (g:+ z (g:sqrt (g:+ :one (g:square z))))))

(define (g:acosh z)
  (g:* :two
       (g:log (g:+ (g:sqrt (g:/ (g:+ z :one) :two))
		   (g:sqrt (g:/ (g:- z :one) :two))))))

(define (g:atanh z)
  (g:/ (g:- (g:log (g:+ :one z))
	    (g:log (g:- :one z)))
       :two))

(define (g:arg-shift f . shifts)
  (define (g . xs)
    (g:apply f (map g:+ xs shifts)))
  g)

(define (g:arg-scale f . scales)
  (define (g . xs)
    (g:apply f (map g:* xs scales)))
  g)


;;bdk;; moved to cstm/generic 11

;;; The generalized selector:


;;bdk;; moved to cstm/mathutil 1

(define ((component . selectors) x)
  (ref-internal x selectors))

;;bdk;; moved to cstm/mathutil 2


(define (g:size x)
  (cond ((vector? x)      (vector-length x))
	((matrix? x)      (matrix-size x))
	((structure? x)   (s:length x))
	((series? x)      #f)
	((stream-pair? x) #f)
	((list? x)        (length x))
	((string? x)      (string-length x))
	(else
	 (error "Unknown compound -- G:size" x))))

;;; Generic composition duplicates composition in utils

(define (g:compose . fs)
  (define (lp fs)
    (cond ((null? (cdr fs)) (car fs))
	  (else (g:compose-2 (car fs) (lp (cdr fs))))))
  (cond ((null? fs) g:identity)
	((null? (cdr fs)) (car fs))
	(else
	 (g:compose-bin (lp (butlast fs))
			(car (last-pair fs))))))

;;bdk;; moved to cstm/generic 10

(define (g:compose-2 f g)
  (cond ((pair? g)
	 (lambda x
	   (g:apply f
		    (map (lambda (gi)
			   (g:apply gi x))
			 g))))
	(else
	 (lambda x
	   (f (g:apply g x))))))

(define (g:compose-bin f g)
  (cond ((and (pair? g) (not (structure? g)))
	 (let ((a
		(a-reduce joint-arity
			  (map g:arity g))))
	   (cond ((equal? a *at-least-zero*)
		  (lambda x
		    (g:apply f
			   (map
			    (lambda (gi)
			      (g:apply gi x))
			    g))))
		 ((equal? a *exactly-zero*)
		  (lambda ()
		    (g:apply f
			   (map (lambda (gi)
				  (gi))
				g))))
		 ((equal? a *at-least-one*)
		  (lambda (x . y)
		    (g:apply f
			   (map (lambda (gi)
				  (g:apply gi x y))
				g))))
		 ((equal? a *exactly-one*)
		  (lambda (x)
		    (g:apply f
			   (map (lambda (gi)
				  (gi x))
				g))))

		 ((equal? a *at-least-two*)
		  (lambda (x y . z)
		    (g:apply f
			   (map (lambda (gi)
				  (g:apply gi x y z))
				g))))
		 ((equal? a *exactly-two*)
		  (lambda (x y)
		    (g:apply f
			   (map (lambda (gi)
				  (gi x y))
				g))))
		 ((equal? a *at-least-three*)
		  (lambda (u x y . z)
		    (g:apply f
			   (map (lambda (gi)
				  (g:apply gi u x y z))
				g))))
		 ((equal? a *exactly-three*)
		  (lambda (x y z)
		    (g:apply f
			   (map (lambda (gi)
				  (gi x y z))
				g))))
		 ((equal? a *one-or-two*)
		  (lambda (x #:optional y)
		    (if (default-object? y)
			(g:apply f
			       (map (lambda (gi)
				      (gi x))
				    g))
			(g:apply f
			       (map (lambda (gi)
				      (gi x y))
				    g)))))
		 (else
		  (lambda x
		    (g:apply f
			   (map
			    (lambda (gi)
			      (g:apply gi x))
			    g)))))))
	(else
	 (let ((a (g:arity g)))
	   (cond ((equal? a *at-least-zero*)
		  (lambda x
		    (g:apply f
			     (list (g:apply g x)))))
		 ((equal? a *exactly-zero*)
		  (lambda ()
		    (g:apply f
			     (list (g:apply g '())))))
		 ((equal? a *at-least-one*)
		  (lambda (x . y)
		    (g:apply f
			     (list (g:apply g x y)))))
		 ((equal? a *exactly-one*)
		  (lambda (x)
		    (g:apply f
			     (list (g:apply g (list x))))))
		 ((equal? a *at-least-two*)
		  (lambda (x y . z)
		    (g:apply f
			     (list (g:apply g x y z)))))
		 ((equal? a *exactly-two*)
		  (lambda (x y)
		    (g:apply f
			     (list (g:apply g (list x y))))))
		 ((equal? a *at-least-three*)
		  (lambda (u x y . z)
		    (g:apply f
			     (list (g:apply g u x y z)))))
		 ((equal? a *exactly-three*)
		  (lambda (x y z)
		    (g:apply f
			     (list (g:apply g (list x y z))))))
		 ((equal? a *one-or-two*)
		  (lambda (x #:optional y)
		    (if (default-object? y)
			(g:apply f
				 (list (g:apply g (list x))))
			(g:apply f
				 (list (g:apply g (list x y)))))))
		 (else
		  (lambda x
		    (g:apply f
			     (list (g:apply g x))))))))))


