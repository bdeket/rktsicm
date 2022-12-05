#lang racket/base

(provide (all-defined-out)
         (all-from-out "cstm/types.rkt")
         operator? differential?)

(require (only-in "../rkt/glue.rkt" fix:= named-lambda)
         "../rkt/applyhook.rkt"
         "../parameters.rkt"
         "cstm/express.rkt"
         "cstm/types.rkt"
         "cstm/s-operator.rkt"
         "cstm/diff.rkt"
         "cstm/generic.rkt")

;;;; This is needed to load particular types

(define (make-type type-tag abstract-type-tag
		   quantity-predicate concrete-predicate abstract-predicate)
  (list type-tag abstract-type-tag
	quantity-predicate concrete-predicate abstract-predicate))

(define (type-tag type)
  (car type))

(define (abstract-type-tag type)
  (cadr type))

(define (quantity-predicate type)
  (caddr type))

(define (concrete-predicate type)
  (cadddr type))

(define (abstract-predicate type)
  (car (cddddr type)))

;;bdk;; part moved to cstm/types

(define (abstract-quantity? x)
  (memq (g:type x) abstract-type-tags))


;;; NUMBER? is defined by Scheme system

(define (abstract-number? x)
  (or (literal-number? x)
      (symbol? x)))

(define (literal-number? x)
  (and (pair? x)
       (eq? (car x) number-type-tag)))

(define (literal-real? x)
  (and (literal-number? x)
       ((has-property? 'real) x)))

(define-values (numerical-quantity? add-to-numerical-quantity?)
  (let ([tests (list number?
                     abstract-number?
                     (λ (x)
                       (and (differential? x)
                            (numerical-quantity? (differential-of x))))
                     #; ;;bdk;; move to units
                     (and (with-units? x)
                          (numerical-quantity? (u:value x))))])
    (values
     (named-lambda (numerical-quantity? x) (for/or ([pred? (in-list tests)]) (pred? x)))
     (λ (test) (set! tests (append tests (list test)))))))

(define (with-units? x)
  (and (pair? x)
       (eq? (car x) with-units-type-tag)))

(define (units? x)
  (or (eq? x '&unitless)
      (and (pair? x)
	   (eq? (car x) unit-type-tag))))


(define *number*
  (make-type '*number* '*number* numerical-quantity? number? abstract-number?))


(define (compound-type-tag? x)
  ;; Will need to add tensors, etc.
  (memq x compound-type-tags))

(define (not-compound? x)
  (not (or (vector? x)
	   (and (pair? x)
		(compound-type-tag? (car x))))))

(define (scalar? x)
  (not (or (vector? x)
	   (and (pair? x)
		(compound-type-tag? (car x)))
	   (function? x)
           (operator? x))))

;;; Scheme vectors are used to represent concrete vectors.
;;; VECTOR? is defined by Scheme system

(define (abstract-vector? x)
  (and (pair? x)
       (eq? (car x) vector-type-tag)))

(define (vector-quantity? v)
  (or (vector? v)
      (abstract-vector? v)
      (and (differential? v)
	   (vector-quantity? (differential-of v)))))


(define *vector*
  (make-type vector-type-tag
	     abstract-vector-type-tag
	     vector-quantity? vector? abstract-vector?))


(define (quaternion? v)
  (and (pair? v)
       (eq? (car v) quaternion-type-tag)))

(define (quaternion-quantity? v)
  (quaternion? v))


(define (up? x)
  ;;(and (pair? x) (eq? (car x) up-type-tag))
  (vector? x))

(define (abstract-up? x)
  (and (pair? x) (eq? (car x) abstract-up-type-tag)))

(define (up-quantity? v)
  (or (up? v)
      (abstract-up? v)
      (and (differential? v)
	   (up-quantity? (differential-of v)))))


(define *up*
  (make-type up-type-tag
	     abstract-up-type-tag
	     vector-quantity? up? abstract-up?))


(define (down? x)
  (and (pair? x)
       (eq? (car x) down-type-tag)))

(define (abstract-down? x)
  (and (pair? x) (eq? (car x) abstract-down-type-tag)))

(define (down-quantity? v)
  (or (down? v)
      (abstract-down? v)
      (and (differential? v)
	   (down-quantity? (differential-of v)))))


(define *down*
  (make-type
   down-type-tag
   abstract-down-type-tag
   down-quantity? down? abstract-down?))

(define (structure? x)
  (or (up? x) (down? x)))


(define (abstract-structure? x)
  (or (abstract-up? x) (abstract-down? x)))

(define (matrix? m)		
  (and (pair? m)
       (eq? (car m) matrix-type-tag)))

(define (matrix-quantity? m)
  (or (matrix? m)
      (abstract-matrix? m)
      (and (differential? m)
	   (matrix-quantity? (differential-of m)))))

(define (abstract-matrix? m)
  (and (pair? m)
       (eq? (car m) abstract-matrix-type-tag)))

(define *matrix*
  (make-type matrix-type-tag
	     abstract-matrix-type-tag
	     matrix-quantity? matrix? abstract-matrix?))

(define (square-matrix? matrix)
  (and (matrix? matrix)
       (fix:= (caadr matrix) (cdadr matrix))))

(define (square-abstract-matrix? matrix)
  (and (pair? matrix)
       (eq? (car matrix) abstract-matrix-type-tag)
       ((has-property? 'square) matrix)))

#; ;;bdk;; moved to cstm/s-operator
(define (operator? x)
  (and (apply-hook? x)
       (eq? (car (apply-hook-extra x))
	    operator-type-tag)))

(define (not-operator? x)
  (not (operator? x)))

(define (function-quantity? f)
  (procedure? f))			;apply hooks are procedures.

(define (function? f)
  (and (procedure? f)
       (not (operator? f))))

(define (cofunction? f)			;may be combined with a function
  (not (operator? f)))

(define (abstract-function? f)
  (and (typed-or-abstract-function? f)
       (f:expression f)))

(define (typed-function? f)
  (and (typed-or-abstract-function? f)
       (not (f:expression f))))

(define (typed-or-abstract-function? f)
  (and (apply-hook? f)
       (eq? (car (apply-hook-extra f))
	    function-type-tag)))

;> originally in litfun.scm
(define (f:expression f)
  (if (typed-or-abstract-function? f)
      (if (*literal-reconstruction*)
	  (cadddr (cdr (apply-hook-extra f)))
	  (cadddr (apply-hook-extra f)))
      #f))

(define *function*
  (make-type function-type-tag
	     abstract-function-type-tag
	     function-quantity? function? abstract-function?))

#; ;;bdk;; moved to cstm/diff
(define (differential? obj)
  (and (pair? obj)
       (eq? (car obj) differential-type-tag)))

(define (not-differential? obj)
  (not (differential? obj)))


(define (series? s)
  (and (pair? s)
       (eq? (car s) series-type-tag)))

(define (not-series? s)
  (not (series? s)))


(define (not-differential-or-compound? x)
  (not (or (vector? x)
	   (and (pair? x)
		(or (compound-type-tag? (car x))
		    (eq? (car x) differential-type-tag))))))

(define (not-d-c-u? x)
  (not (or (eq? x '&unitless)
	   (vector? x)
	   (and (pair? x)
		(or (compound-type-tag? (car x))
		    (eq? (car x) differential-type-tag)
		    (eq? (car x) with-units-type-tag)
		    (eq? (car x) unit-type-tag))))))

