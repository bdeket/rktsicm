#lang s-exp "extapply.rkt"

(provide (except-out (all-defined-out) assign-operation)
         (all-from-out "cstm/s-operator.rkt"))

(require (only-in racket/list take)
         (only-in "../rkt/define.rkt" define default-object?)
         "../rkt/applyhook.rkt"
         "../general/assert.rkt"
         "../general/sets.rkt"
         (rename-in (only-in racket/base equal?) [equal? pair:eq?])
         "cstm/s-operator.rkt"
         "express.rkt"
         "types.rkt"
         "utils.rkt"
         "generic.rkt"
         "mathutil.rkt"
         "function.rkt"
         "cstm/pseries.rkt"
         )
(define-values (assign-operation operator:assign-operations)
  (make-assign-operations 'operator))

;;bdk;; start original file

;;;; Operators



(define (o:type o) operator-type-tag)

(define (o:type-predicate o) operator?)

(define (o:arity o) 
  (operator-arity o))


#|;;; In GENERIC.SCM

(define (make-operator p #:optional name subtype arity . opts)
  (if (default-object? name) (set! name #f))
  (if (default-object? subtype) (set! subtype #f))
  (if (default-object? arity) (set! arity (procedure-arity p)))
  (make-apply-hook p `(,operator-type-tag ,subtype ,name ,arity ,@opts)))
|#

;;bdk;; moved to cstm/s-operator 1

;;bdk;; moved to cstm/s-operator 3

(define (simple-operator? op)
  (and (operator? op)
       (not (operator-subtype op))))

(define (set-operator-optionals! op value)
  (assert (operator? op))
  (set-apply-hook-extra! op (append (take (apply-hook-extra op) 4) value)) #;
  (set-cdr! (cdddr (apply-hook-extra op)) value)
  op)

(define (operator-merge-subtypes op1 op2)
  (let ((t1 (operator-subtype op1))
	(t2 (operator-subtype op2)))
    (cond ((eq? t1 t2) t1)
	  ((not t1)    t2)
	  ((not t2)    t1)
	  (else
	   (error "Incompatible subtypes -- OPERATOR" t1 t2)))))

(define (operator-merge-arities op1 op2)
  (joint-arity (operator-arity op1) (operator-arity op2)))

(define (operator-merge-optionals op1 op2)
  (list-union (operator-optionals op1)
	      (operator-optionals op2)))



(define (o:zero-like op)
  (assert (pair:eq? (operator-arity op) *exactly-one*) "o:zero-like")
  (make-op (lambda (f) (g:zero-like f))
	   'zero
	   (operator-subtype op)
	   (operator-arity op)
	   (operator-optionals op)))

(define (o:one-like op)
  (assert (pair:eq? (operator-arity op) *exactly-one*) "o:one-like")
  (make-op g:identity
	   'identity
	   (operator-subtype op)
	   (operator-arity op)
	   (operator-optionals op)))

(define o:identity
  (make-operator g:identity 'identity))

(define (o:+ op1 op2)
  (make-op (lambda fs
	     (g:+ (apply op1 fs) (apply op2 fs)))
	   `(+ ,(operator-name op1)
	       ,(operator-name op2))
	   (operator-merge-subtypes op1 op2)
	   (operator-merge-arities op1 op2)
	   (operator-merge-optionals op1 op2)))

(define (o:- op1 op2)
  (make-op (lambda fs
	     (g:- (apply op1 fs) (apply op2 fs)))
	   `(- ,(operator-name op1)
	       ,(operator-name op2))
	   (operator-merge-subtypes op1 op2)
	   (operator-merge-arities op1 op2)
	   (operator-merge-optionals op1 op2)))

(define (o:o+f op f)
  (let ((h (coerce-to-function f)))
    (make-op (lambda (g)
	       (g:+ (op g)
		    (g:compose h g)))
	     `(+ ,(operator-name op) ,(procedure-expression h))
	     (operator-subtype op)
	     (operator-arity op)
	     (operator-optionals op))))

(define (o:f+o f op)
  (let ((h (coerce-to-function f)))
    (make-op (lambda (g)
	       (g:+ (g:compose h g)
		    (op g)))
	     `(+ ,(procedure-expression h) ,(operator-name op))
	     (operator-subtype op)
	     (operator-arity op)
	     (operator-optionals op))))

(define (o:o-f op f)
  (let ((h (coerce-to-function f)))
    (make-op (lambda (g)
	       (g:- (op g)
		    (g:compose h g)))
	     `(- ,(operator-name op) ,(procedure-expression h))
	     (operator-subtype op)
	     (operator-arity op)
	     (operator-optionals op))))

(define (o:f-o f op)
  (let ((h (coerce-to-function f)))
    (make-op (lambda (g)
	       (g:- (g:compose h g)
		    (op g)))
	     `(- ,(procedure-expression h) ,(operator-name op))
	     (operator-subtype op)
	     (operator-arity op)
	     (operator-optionals op))))

(define (o:negate op)
  (make-op (lambda fs
	     (g:negate (apply op fs)))
	   `(- ,(operator-name op))
	   (operator-subtype op)
	   (operator-arity op)
	   (operator-optionals op)))

(define (o:* op1 op2)
  (let ((subtype
	 (operator-merge-subtypes op1 op2)))
    (if (procedure? subtype)
	(subtype op1 op2)
	(make-op (compose op1 op2)
		 `(* ,(operator-name op1)
		     ,(operator-name op2))
		 subtype
		 (operator-arity op2)
		 (operator-merge-optionals op1 op2)))))

(define (o:f*o f op)
  (make-op (lambda gs
	     (g:* f (apply op gs)))
	   `(* ,(procedure-expression
		 (coerce-to-function f))
	       ,(operator-name op))
	   (operator-subtype op)
	   (operator-arity op)
	   (operator-optionals op)))

(define (o:o*f op f)
  (make-op (lambda gs
	     (apply op (map (lambda (g) (g:* f g)) gs)))
	   `(* ,(operator-name op)
	       ,(procedure-expression
		 (coerce-to-function f)))
	   (operator-subtype op)
	   (operator-arity op)
	   (operator-optionals op)))

(define (o:o/n op n)
  (make-op (lambda gs
	     (g:* (/ 1 n) (apply op gs)))
	   `(/ ,(operator-name op) ,n)
	   (operator-subtype op)
	   (operator-arity op)
	   (operator-optionals op)))

(define (o:expt op n)
  (assert (pair:eq? (operator-arity op) *exactly-one*) "o:expt")
  (make-op (iterated op n o:identity)
	   `(expt ,(operator-name op) ,n)
	   (operator-subtype op)
	   (operator-arity op)
	   (operator-optionals op)))

(define (o:exp op)
  (assert (pair:eq? (operator-arity op) *exactly-one*) "o:exp")
  (make-op (lambda (g)
	     (lambda x
	       (g:apply ((series:value exp-series (list op)) g) x)))
	   `(exp ,(operator-name op))
	   (operator-subtype op)
	   (operator-arity op)
	   (operator-optionals op)))

(define (o:cos op)
  (assert (pair:eq? (operator-arity op) *exactly-one*) "o:cos")
  (make-op (lambda (g)
	     (lambda x
	       (g:apply ((series:value cos-series (list op)) g) x)))
	   `(cos ,(operator-name op))
	   (operator-subtype op)
	   (operator-arity op)
	   (operator-optionals op)))

(define (o:sin op)
  (assert (pair:eq? (operator-arity op) *exactly-one*) "o:sin")
  (make-op (lambda (g)
	     (lambda x
	       (g:apply ((series:value sin-series (list op)) g) x)))
	   `(sin ,(operator-name op))
	   (operator-subtype op)
	   (operator-arity op)
	   (operator-optionals op)))


;;; Optional order argument for exponentiation of operators.
;;; (((expn D 2) g) x)
;;;   = (((exp D)
;;;       (lambda (eps)
;;;        (((+ 1 (* (expt eps 2) D) (* 1/2 (expt eps 4) (expt D 2)) ...) g) x))
;;;      0)
;;; This is (exp (* (expt eps 2) D)) written as a power series in eps.

(define (expn op #:optional exponent)
  (assert (operator? op))
  (assert (pair:eq? (operator-arity op) *exactly-one*) "o:expn")
  (if (default-object? exponent)
      (o:exp op)
      (make-op
       (lambda (g)
	 (lambda x
	   (g:apply ((series:inflate (series:value exp-series (list op))
				      exponent)
		     g)
		    x)))
       `(exp ,(operator-name op))
       (operator-subtype op)
       (operator-arity op)
       (operator-optionals op))))


(assign-operation 'type                o:type            operator?)
(assign-operation 'type-predicate      o:type-predicate  operator?)
(assign-operation 'arity               o:arity           operator?)

(assign-operation 'zero-like o:zero-like simple-operator?)
(assign-operation 'one-like o:one-like operator?)
(assign-operation 'identity-like o:one-like operator?)

(assign-operation '+          o:+               operator? operator?) 
(assign-operation '+          o:o+f             operator? not-operator?) 
(assign-operation '+          o:f+o             not-operator? operator?) 

(assign-operation '-          o:-               operator? operator?)
(assign-operation '-          o:o-f             operator? not-operator?) 
(assign-operation '-          o:f-o             not-operator? operator?) 

(assign-operation '*          o:*               operator? operator?)
(assign-operation '*          o:o*f             operator? not-operator?) 
(assign-operation '*          o:f*o             not-operator? operator?)
(assign-operation '/          o:o/n             operator? numerical-quantity?)


(assign-operation 'solve-linear-right   o:o/n                    operator? numerical-quantity?)
(assign-operation 'solve-linear-left    (lambda (x y) (o:o/n y x))    numerical-quantity? operator?)
(assign-operation 'solve-linear         (lambda (x y) (o:o/n y x))    numerical-quantity? operator?)

(assign-operation 'negate     o:negate          operator?)
(assign-operation 'expt       o:expt            operator? exact-integer?)

(assign-operation 'exp                 o:exp             operator?)
(assign-operation 'sin                 o:sin             operator?)
(assign-operation 'cos                 o:cos             operator?)

