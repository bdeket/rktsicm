#lang racket/base

(provide (all-defined-out))

(require "types.rkt")


;;bdk;; insert 1
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define first-operand cadr)
(define second-operand caddr)
(define rest-operands cddr)
;;bdk;; insert 1 end

;;; Abstract quantities are represented with a type-tagged property list,
;;; implemented as an alist.

;;bdk;; change this to hash, since racket doesn't use mlists by default
;;bdk;; abstract-quantity needs to be (or/c symbol? (pair? symbol? hash?))

;;bdk;; insert 2
(define ((has-property? property-name) abstract-quantity)
  (cond
    [(pair? abstract-quantity)
     (if (hash? (cdr abstract-quantity))
         (hash-ref (cdr abstract-quantity) property-name #f)
         (error "Malformed abstract-quantity" abstract-quantity))]
    [(symbol? abstract-quantity)
     (if (eq? property-name 'expression)
         abstract-quantity
         (error "Symbols have only EXPRESSION properties"))]
    [else
     (error "Bad abstract quantity")]))

(define (get-property abstract-quantity property-name [default #f])
  (cond
    [(pair? abstract-quantity)
     (hash-ref (cdr abstract-quantity) property-name default)]
    [(symbol? abstract-quantity)
     (if (eq? property-name 'expression)
         abstract-quantity
         default)]
    [else
     (error "Bad abstract quantity")]))
	 
;;bdk;; this is potentially different: originally new properties were added
;;bdk;; at the back of the list (even if they already existed)
;;bdk;; so a lookup would find the first added, not the last
;;bdk;; TODO: check this works ok

(define (add-property! abstract-quantity property-name property-value)
  (if (pair? abstract-quantity)
      (hash-set! (cdr abstract-quantity) property-name property-value)
      (error "Bad abstract quantity -- ADD-PROPERTY!")))
;;bdk;; insert 2 end



;;bdk;; insert 3
;;; An abstract quantity may be have a type-tagged expression.

(define (make-numerical-literal expression)
  (make-literal number-type-tag expression))

(define (make-real-literal expression)
  (let ((e (make-numerical-literal expression)))
    (add-property! e 'real #t)
    e))

(define (make-literal type-tag expression)
  (cons type-tag (make-hasheq (list (cons 'expression expression)))))
(define (make-combination type-tag operator operands) 
  (make-literal type-tag (cons operator operands)))

(define (expression-of abstract-quantity)
  (cond ((pair? abstract-quantity)
	 (let ((v (get-property abstract-quantity 'expression)))
	   (or v
	       (error "No expression for abstract quantity"
		      abstract-quantity))))
	((symbol? abstract-quantity)
	 abstract-quantity)
	(else
	 (error "Bad abstract quantity"))))
;;bdk;; insert 3 end