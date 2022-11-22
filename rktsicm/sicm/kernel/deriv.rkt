#lang racket/base

(provide (except-out (all-defined-out) assign-operation))

(require "../general/logic-utils.rkt"
         "../rkt/default-object.rkt"
         "diff.rkt"
         "generic.rkt"
         "mathutil.rkt"
         "numeric.rkt"
         "structs.rkt"
         "types.rkt"
         "utils.rkt"
         )
(define-values (assign-operation deriv:assign-operations)
  (make-assign-operations 'deriv))

;;;;                 General Derivative Procedures

;;;     In DIFF.SCM we define the primitive mechanism extending the
;;; generic operators for differential quantities.  We also defined
;;; DIFF:DERIVATIVE, the procedure that produces the derivative
;;; function of a real-valued function of a real argument.  Here we
;;; use this mechanism to build derivatives of systems with
;;; structured arguments and structured values.  The basic rule is
;;; that a derivative function produces a value which may be
;;; multiplied by an increment of the argument to get a linear
;;; approximation to the increment in the function.

;;; Let's start with functions on Euclidean space.  We create a
;;; Euclidean-space derivative so that we may pass in an arbitrary
;;; structure of nested vectors and covectors.
;;;
;;;           f
;;;     R^n -----> R^m
;;;
;;; The derivative Df of this function is a function defined on R^n.
;;; It maps incremental vectors in R^n to incremental vectors in R^m.
;;;
;;;           Df          df
;;;     R^n -----> (R^n -----> R^m)
;;;
;;; It is only for these Euclidean spaces that we can identify the
;;; manifold with its tangent space at each point.  This will be a
;;; problem we will get back to later.  Note that df is a linear
;;; function, so it can be represented by an mXn matrix. (That is,
;;; one with m rows and n columns.)

;;; Note: it makes no difference if the deriv:euclidean-structure is
;;; linear-memoized... Never get a cache hit.

(define (deriv:euclidean-structure f selectors)
  (define (sd g v)
    (cond ((structure? v)
	   (s:generate (s:length v) (s:opposite v)
		       (lambda (i)
			 (sd (lambda (xi)
			       (g (s:with-substituted-coord v i xi)))
			     (s:ref v i))))) 
	  ((or (numerical-quantity? v) (abstract-quantity? v))
	   (simple-derivative-internal g v))
	  (else
	   (error "Bad structure -- DERIV:EUCLIDEAN-STRUCTURE" g v))))
  (define (a-euclidean-derivative v)
    (cond ((structure? v)
	   (sd (lambda (w)
		 (g:apply f (list (s:subst-internal v w selectors))))
	       (ref-internal v selectors)))
	  ((null? selectors)
	   (simple-derivative-internal f v))
	  (else
	   (error "Bad selectors -- DERIV:EUCLIDEAN-STRUCTURE"
		  f selectors v))))
  a-euclidean-derivative)

#|
;;; An old failed experiment...

(define (deriv:euclidean-structure f)
  (define (sd g v)
    (cond ((structure? v)
	   (s:generate (s:length v) (s:opposite v)
		       (lambda (i)
			 (sd (lambda (xi)
			       (g (s:with-substituted-coord v i xi)))
			     (s:ref v i))))) 
	  ((or (numerical-quantity? v)
	       (abstract-quantity? v))
	   (simple-derivative-internal g v))
	  (else
	   (error "Bad structure -- DERIV:EUCLIDEAN-STRUCTURE"
		  g v))))
  (define (a-euclidean-derivative v)
    (fluid-let ((differential-tag-count differential-tag-count))
      (sd f v)))
  a-euclidean-derivative)

;;; The fluid let greatly improves the efficiency of the system by
;;; reducing more intermediate expressions to a canonical form, but it
;;; causes the following bug:

(pe ((simple-derivative-internal
      (lambda (eps)
	 (lambda (t)
	   ((D (* cos eps)) t)))
      'e)
     't))
(* -1 (sin t)) ;; correct


(pe (((D
       (lambda (eps)
	 (lambda (t)
	   ((D (* cos eps)) t))))
      'e)
     't))
0	      ;; wrong!

;;; To recover this idea see custom-repl.scm
|#

;;; Once we have this, we can implement derivatives of multivariate
;;; functions by wrapping their arguments into an UP-STRUCTURE for
;;; differentiation by DERIV:EUCLIDEAN-STRUCTURE.  This code sucks!

(define (deriv:multivariate-derivative f selectors)
  (let ((a (g:arity f))
	(d (lambda (f) (deriv:euclidean-structure f selectors))))
    (cond ((equal? a *exactly-zero*)
	   (lambda () :zero))
	  ((equal? a *at-least-one*)
	   (lambda (x . y)
	     ((d (lambda (s) (g:apply f (up-structure->list s))))
	      (list->up-structure (cons x y)))))
	  ((equal? a *exactly-one*)
	   (d f))
	  ((equal? a *at-least-two*)
	   (lambda (x y . z)
	     ((d (lambda (s) (g:apply f (up-structure->list s))))
	      (list->up-structure (list* x y z)))))
	  ((equal? a *exactly-two*)
	   (lambda (x y)
	     ((d (lambda (s) (g:apply f (up-structure->list s))))
	      (list->up-structure (list x y)))))
	  ((equal? a *at-least-three*)
	   (lambda (u x y . z)
	     ((d (lambda (s) (g:apply f (up-structure->list s))))
	      (list->up-structure (list* u x y z)))))
	  ((equal? a *exactly-three*)
	   (lambda (x y z)
	     ((d (lambda (s) (g:apply f (up-structure->list s))))
	      (list->up-structure (list x y z)))))
	  ((equal? a *one-or-two*)
	   (lambda (x [y default-object])
	     (if (default-object? y)
		 ((d f) x)
		 ((d (lambda (s)
		       (g:apply f (up-structure->list s))))
		  (list->up-structure (list x y))))))
	  (else
	   (lambda args
	     (cond ((null? args)
		    (error "No args passed to derivative?")
		    0)
		   ((null? (cdr args))	; one argument
		    ((d f) (car args)))
		   (else
		    ((d (lambda (s)
			  (g:apply f (up-structure->list s))))
		     (list->up-structure args)))))))))

(assign-operation 'partial-derivative
		  deriv:multivariate-derivative
		  (disjunction function? structure?)
		  any?)

;;; In order to implement derivatives with respect to abstract
;;; quantities we need to create more types -- differential vector,
;;; differential matrix, etc?  Let's attack that later.
