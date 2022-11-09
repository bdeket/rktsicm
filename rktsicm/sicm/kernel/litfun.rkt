#lang racket/base

(provide (all-defined-out)
         *literal-reconstruction*
         f:expression)

(require (only-in racket/list make-list)
         racket/fixnum
         (only-in "../rkt/racket-help.rkt" symbol)
         "../rkt/applyhook.rkt"
         "../general/logic-utils.rkt"
         "../parameters.rkt"
         "diff.rkt"
         "cstm/express.rkt"
         "cstm/generic-apply.rkt"
         "numsymb.rkt"
         "structs.rkt"
         "types.rkt"
         "utils.rkt"
         )

;;;; Literal function descriptor language.
;;;  This file is case sensitive.

;;; The descriptors for literal functions look like prefix versions of
;;; the standard function types.  Thus, we want to be able to say:

;;;        (literal-function 'V (-> (X Real Real) Real))

;;; The base types are the real numbers, designated by "Real".  We
;;; will later extend the system to include complex numbers,
;;; designated by "Complex".

;;; Types can be combined in several ways.  The cartesian product of
;;; types is designated by:
;;;   (X <type1> <type2> ...)
;;; We use this to specify an argument tuple of objects of the given
;;; types arranged in the given order.

;;; Similarly, we can specify an up tuple or a down tuple with:
;;;   (UP <type1> <type2> ...)
;;;   (DOWN <type1> <type2> ...)

;;; We can also specify a uniform tuple of a number of elements of the
;;; same type using:
;;;   (UP* <type> [n])
;;;   (DOWN* <type> [n])

#|
;;; So, for example:

(define H
  (literal-function 'H
		    (-> (UP Real (UP* Real 2) (DOWN* Real 2)) Real)))

(show-expression 
 (((Hamilton-equations H)
   (coordinate-tuple (literal-function 'x)
		     (literal-function 'y))
   (momentum-tuple (literal-function 'p_x)
		   (literal-function 'p_y)))
  't))
(up
 0
 (up
  (+ ((D x) t)
     (* -1
	(((partial 2 0) H) (up t (up (x t) (y t)) (down (p_x t) (p_y t))))))
  (+ ((D y) t)
     (* -1
	(((partial 2 1) H) (up t (up (x t) (y t)) (down (p_x t) (p_y t)))))))
 (down
  (+ ((D p_x) t)
     (((partial 1 0) H) (up t (up (x t) (y t)) (down (p_x t) (p_y t)))))
  (+ ((D p_y) t)
     (((partial 1 1) H) (up t (up (x t) (y t)) (down (p_x t) (p_y t)))))))
|#

(install-g:apply-case applicable-literal?
                      (λ (f args)
                        (apply
                         (literal-function f
                                           (permissive-function-type (length args)))
                         args)))

;;; To get started... Type expressions are self-evaluating

(define Real 'Real)

(define (X . types)
  (cond ((null? types) (error "Null type argument -- X"))
	((null? (cdr types)) (car types))
	(else (cons 'X types))))

(define (UP . types)
  (cond ((null? types) (error "Null type argument -- UP"))
	((null? (cdr types)) (car types))
	(else (cons 'UP types))))

(define (DOWN . types)
  (cond ((null? types) (error "Null type argument -- DOWN"))
	((null? (cdr types)) (car types))
	(else (cons 'DOWN types))))

(define (^ type n)			;n = dimension
  (apply X (make-list n type)))


(define (starify rest starred unstarred-proc)
  (cond ((null? rest) (error "Null type argument" starred))
	(else
	 (let lp ((args rest) (curtype #f) (explicit #f) (types '()))
	   (cond ((null? args)
		  (if explicit (apply unstarred-proc types) (cons starred types)))
		 ((exact-positive-integer? (car args))
		  (if curtype
		      (lp (cdr args)
			  #f
			  #t
			  (append types (make-list (fx- (car args) 1) curtype)))
		      (error "Bad type arguments" starred rest)))
		 (else
		  (lp (cdr args)
		      (car args)
		      #f
		      (append types (list (car args))))))))))


(define (X* . rest)
  (starify rest 'X* X))

(define (UP* . rest)
  (starify rest 'UP* UP))

(define (DOWN* . rest)
  (starify rest 'DOWN* DOWN))


(define (-> domain range)
  `(-> ,domain ,range))


(define Any 'Any)

(define (default-function-type n [type Real])
  (if (= n 1)
      '(-> Real Real)
      (-> (X* Real n) Real)))

(define (permissive-function-type n)
  (-> (X* Any n) Real))


;;; Some useful types

(define (Lagrangian [n 1])	;n = #degrees-of-freedom
  (if (= n 1)
      (-> (UP* Real (UP* Real) (UP* Real)) Real)
      (-> (UP Real (UP* Real n) (UP* Real n)) Real)))

(define (Hamiltonian [n 1])	;n = #degrees-of-freedom
  (if (= n 1)
      (-> (UP Real (UP* Real) (DOWN* Real)) Real)
      (-> (UP Real (UP* Real n) (DOWN* Real n)) Real)))


#| ;;; For example

(define L (literal-function 'L (Lagrangian)))

(pe (L (->L-state 't 'x 'v)))
(L (up t x v))

(pe ((D L) (->L-state 't 'x 'v)))
(down (((partial 0) L) (up t x v))
      (((partial 1) L) (up t x v))
      (((partial 2) L) (up t x v)))

(pe (L (->L-state 't (up 'x 'y) (up 'v_x 'v_y))))
(L (up t (up x y) (up v_x v_y)))

(pe ((D L) (->L-state 't (up 'x 'y) (up 'v_x 'v_y))))
(down
 (((partial 0) L) (up t (up x y) (up v_x v_y)))
 (down (((partial 1 0) L) (up t (up x y) (up v_x v_y)))
       (((partial 1 1) L) (up t (up x y) (up v_x v_y))))
 (down (((partial 2 0) L) (up t (up x y) (up v_x v_y)))
       (((partial 2 1) L) (up t (up x y) (up v_x v_y)))))


(define H (literal-function 'H (Hamiltonian)))

(pe (H (->H-state 't 'x 'p)))
(H (up t x p))

(pe ((D H) (->H-state 't 'x 'p)))
(down (((partial 0) H) (up t x p))
      (((partial 1) H) (up t x p))
      (((partial 2) H) (up t x p)))

(pe (H (->H-state 't (up 'x 'y) (down 'p_x 'p_y))))
(H (up t (up x y) (down p_x p_y)))

(pe ((D H) (->H-state 't (up 'x 'y) (down 'p_x 'p_y))))
(down
 (((partial 0) H) (up t (up x y) (down p_x p_y)))
 (down (((partial 1 0) H) (up t (up x y) (down p_x p_y)))
       (((partial 1 1) H) (up t (up x y) (down p_x p_y))))
 (up (((partial 2 0) H) (up t (up x y) (down p_x p_y)))
     (((partial 2 1) H) (up t (up x y) (down p_x p_y)))))
|#


;;;---------------------------------------------------------------------

(define (type->domain type)
  (assert (eq? (car type) '->))
  (cadr type))

(define (type->range-type type)
  (assert (eq? (car type) '->))
  (caddr type))

(define (type->domain-types type)
  (assert (eq? (car type) '->))
  (let ((domain (type->domain type)))
    (cond ((and (pair? domain) (eq? (car domain) 'X))
	   (cdr domain))
	  (else
	   (list domain)))))

(define (type->arity type)
  (assert (eq? (car type) '->))
  (let ([domain (type->domain type)])
    (cond
      [(and (pair? domain) (eq? (car domain) 'X))
       (length->exact-arity (length (cdr domain)))]
      [(and (pair? domain) (eq? (car domain) 'X*))
       (arity-at-least 0)]
      [else
       (length->exact-arity 1)])))

(define (length->exact-arity n)
  (assert (exact-integer? n))
  (cons n n))


(define (type-expression->predicate type-expression)
  (cond ((pair? type-expression)
	 (case (car type-expression)
	   ((X)
	    (let ((type-predicates
		   (map type-expression->predicate
			(cdr type-expression))))
	      (lambda (datum)
		(and (vector? datum)
		     (all-satisfied type-predicates datum)))))
	   ((UP)
	    (let ((type-predicates
		   (map type-expression->predicate
			(cdr type-expression))))
	      (lambda (datum)
		(and (up? datum)
		     (all-satisfied type-predicates datum)))))
	   ((DOWN)
	    (let ((type-predicates
		   (map type-expression->predicate
			(cdr type-expression))))
	      (lambda (datum)
		(and (down? datum)
		     (all-satisfied type-predicates datum)))))
	   ((X*)
	    (let ((type-predicates
		   (map type-expression->predicate
			(cdr type-expression))))
	      (lambda (datum)
		(cond ((vector? datum)
		       (let ((n (vector-length datum)))
			 (let lp ((i 0) (preds type-predicates))
			   (cond ((fx= i n) #t)
				 (((car preds) (vector-ref datum i))
				  (lp (fx+ i 1)
				      (if (null? (cdr preds))
					  preds
					  (cdr preds))))
				 (else #f)))))
		      ((null? (cdr type-predicates))
		       ((car type-predicates) datum))
		      (else #f)))))
	   ((UP* DOWN*)
	    (let ((type-predicates
		   (map type-expression->predicate
			(cdr type-expression)))
		  (test?
		   (if (eq? (car type-expression) 'UP*) up? down?)))
	      (lambda (datum)
		(cond ((test? datum)
		       (let ((n (s:length datum)))
			 (let lp ((i 0) (preds type-predicates))
			   (cond ((fx= i n) #t)
				 (((car preds) (s:ref datum i))
				  (lp (fx+ i 1)
				      (if (null? (cdr preds))
					  preds
					  (cdr preds))))
				 (else #f)))))
		      ((and (not (structure? datum))
			    (null? (cdr type-predicates)))
		       ((car type-predicates) datum))
		      (else #f)))))
	   ((->) function?)
	   (else (error "Unknown type combinator" type-expression))))
	((eq? type-expression Real) numerical-quantity?)
	((eq? type-expression Any) any?)
	(else (error "Unknown primitive type" type-expression))))


(define (all-satisfied type-preds structure)
  (let ((n (length type-preds)))
    (and (fx= n (s:length structure))
	 (let lp ((types type-preds) (i 0))
	   (cond ((fx= i n) #t)
		 (((car types) (s:ref structure i))
		  (lp (cdr types) (fx+ i 1)))
		 (else #f))))))

(define (type-expression->type-tag type-expression)
  (let ((type
	 (cond ((pair? type-expression)
		(case (car type-expression)
		  ((X) *vector*)
		  ((UP) *up*)
		  ((DOWN) *down*)
		  ((X*) *vector*)
		  ((UP*) *up*)
		  ((DOWN*) *down*)
		  ((->) *function*)
		  (else
		   (error "Unknown type combinator" type-expression))))
	       ((eq? type-expression Real)
		*number*)
	       (else
		(error "Unknown primitive type" type-expression)))))
    (abstract-type-tag type)))


;;; For computing the type of the range of the derivative of a
;;;  function with a given type.

(define (df-range-type f-domain-types f-range-type arg)
  ;; There is some idea here that I should do something like
  ;; (type-complement (type-expression arg) f-range-type)
  ;; but the argument currently escapes me as to why I need this.
  f-range-type)


;;; Functions with types are defined as apply hooks...

(define (f:domain-types f)
  (if (typed-or-abstract-function? f)
      (cadr (apply-hook-extra f))
      #f))

(define (f:range-type f)
  (if (typed-or-abstract-function? f)
      (caddr (apply-hook-extra f))
      #f))

(define (typed-function function range-type domain-types)
  (let ((arity (g:arity function)))
    (assert (exactly-n? arity)
	    "I cannot handle this arity -- TYPED-FUNCTION")
    (assert (fx= (length domain-types) (car arity))
	    "Inconsistent arity -- TYPED-FUNCTION")
    (make-apply-hook function
                     (list '*function* domain-types range-type #f))))


(define (literal-function? f)
  (and (apply-hook? f)
       (eq? (car (apply-hook-extra f)) '*function*)))

(define (literal-function fexp [descriptor (default-function-type 1)])
  (let ((arity (type->arity descriptor))
	(range-type (type->range-type descriptor)))
    (cond ((or (eq? Real range-type)
	       (eq? '*function* (type-expression->type-tag range-type)))
	   (litfun fexp arity range-type (type->domain-types descriptor)
		   `(literal-function ',fexp ,descriptor)))
	  ((not (symbol? fexp))
	   (error "Cannot handle this function expression: LITERAL-FUNCTION"
		  fexp
		  descriptor))
	  ((eq? (car range-type) 'UP)
	   (let ((n (length (cdr range-type))))
	     (s:generate n 'up
			 (lambda (i)
			   (literal-function (symbol fexp '^ i)
					     (-> (type->domain descriptor)
						 (list-ref (cdr range-type) i)))))))
	  ((eq? (car range-type) 'DOWN)
	   (let ((n (length (cdr range-type))))
	     (s:generate n 'down
			 (lambda (i)
			   (literal-function (symbol fexp '_ i)
					     (-> (type->domain descriptor)
						 (list-ref (cdr range-type) i)))))))
	  (else
	   (error "Cannot handle this range type: LITERAL-FUNCTION"
		  fexp
		  descriptor)))))

(define (litfun fexp arity range-type domain-types call)
  ;;(assert (exactly-n? arity)
  ;;        "I cannot handle this arity -- LITERAL-FUNCTION")
  (define apply-hook
    (make-apply-hook
     (procedure-rename
      (cond ((equal? arity *exactly-zero*)
             (lambda () (literal-apply apply-hook '())))
            ((equal? arity *exactly-one*)
             (lambda (x) (literal-apply apply-hook (list x))))
            ((equal? arity *exactly-two*)
             (lambda (x y) (literal-apply apply-hook (list x y))))
            ((equal? arity *exactly-three*)
             (lambda (x y z) (literal-apply apply-hook (list x y z))))
            (else
             (lambda args (literal-apply apply-hook args))))
      'literal-function)
     (list '*function* domain-types range-type fexp call)))
  apply-hook)


(define (literal-apply apply-hook args)
  (if (rexists differential? args)
      (litderiv apply-hook args)
      (let ((fexp (f:expression apply-hook))
	    (dtypes (f:domain-types apply-hook))
	    (rtype (f:range-type apply-hook)))
	(let ((dpreds (map type-expression->predicate dtypes))
	      (range-tag (type-expression->type-tag rtype)))
	  (assert (&and (map (lambda (p x) (p x)) dpreds args))
		  "Wrong type argument -- LITERAL-FUNCTION"
		  (cons fexp args))
	  (if (eq? range-tag '*function*)
	      (let ((ans (literal-function `(,fexp ,@args) rtype)))
		;; properties?
		ans)	  
	      (let ((ans (make-combination range-tag fexp args)))
		(add-property! ans 'literal-function apply-hook)
		(add-property! ans 'type-expression rtype)
		ans))))))

(define (litderiv apply-hook args)
  (let ((v (list->up-structure args)))
    (let ((maxtag (apply max-order-tag (s:fringe v))))
      (let ((ev
	     (up-structure->list
	      (s:map/r (lambda (x) (without-tag x maxtag)) v)))
	    (dv
	     (s:map/r (lambda (x) (with-tag x maxtag)) v)))
	(d:+ (apply apply-hook ev)
	     (a-reduce d:+
		       (map (lambda (partialx dx)
			      (d:* (apply partialx ev) dx))
			    (s:fringe (make-partials apply-hook v))  
			    (s:fringe dv))))))))


(define (make-partials apply-hook v)
  (define (fd indices vv)
    (cond ((structure? vv)
	   (s:generate (s:length vv) (s:same vv)
		       (lambda (i)
			 (fd (cons i indices)
			     (s:ref vv i))))) 
	  ((or (numerical-quantity? vv)
	       (abstract-quantity? vv))
	   (let ((fexp		  
		  (let ((is (reverse indices)))
		    (if (equal? (g:arity apply-hook) *exactly-one*) ;univariate
			(if (fx= (car is) 0)
			    (if (fx= (length indices) 1)
				(symb:derivative (f:expression apply-hook))
				`((partial ,@(cdr is))
				  ,(f:expression apply-hook)))
			    (error "Wrong indices -- MAKE-PARTIALS"
				   indices vv))
			`((partial ,@is)
			  ,(f:expression apply-hook)))))
		 (range
		  (df-range-type (f:domain-types apply-hook)
				 (f:range-type apply-hook)
				 vv))
		 (domain
		  (f:domain-types apply-hook)))
	     (litfun fexp
		     (g:arity apply-hook)
		     range
		     domain
		     `(literal-function ',fexp
					(-> ,(apply X domain) ,range)))))
	  (else
	   (error "Bad structure -- MAKE-PARTIALS"
		  indices vv))))
  (fd '() v))


#|
;;; Not used anywhere.

(define (accumulate-tags v)
  (cond ((structure? v)
	 (let ((n (s:length v)))
	   (let lp ((i 0) (ut '()))
	     (if (fx= i n)
		 ut
		 (lp (fx+ i 1)
		     (union-differential-tags
		      ut
		      (accumulate-tags (s:ref v i))))))))
	((numerical-quantity? v)
	 (differential-tags
	  (car (last-pair (differential->terms v)))))
	(else
	 (error "Bad structure -- ACCUMULATE-TAGS" v))))
|#