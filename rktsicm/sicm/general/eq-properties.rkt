#lang racket/base

(provide (all-defined-out))

(require "list-utils.rkt")


;;;; Traditional LISP property lists
;;; extended to work on any kind of eq? data structure.

;;; Property lists are a way of creating data that looks like a record
;;; structure without commiting to the fields that will be used until
;;; run time.  The use of such flexible structures is frowned upon by
;;; most computer scientists, because it is hard to statically
;;; determine the bounds of the behavior of a program written using
;;; this stuff.  But it makes it easy to write programs that confuse
;;; such computer scientists.  I personally find it difficult to write
;;; without such crutches.  -- GJS

(define (assoc-del itm lst [is-equal? equal?])
  (let loop ([lst lst])
    (cond
      [(null? lst) lst]
      [(is-equal? (caar lst) itm)
       (loop (cdr lst))]
      [else (cons (car lst) (loop (cdr lst)))])))
(define (assq-del itm lst) (assoc-del itm lst eq?))

(define (assoc-del* itms lst [is-equal? equal?])
  (let loop ([lst lst])
    (cond
      [(null? lst) lst]
      [(ormap (λ (x) (is-equal? (caar lst) x)) itms)
       (loop (cdr lst))]
      [else (cons (car lst) (loop (cdr lst)))])))
(define (assq-del* itms lst) (assoc-del itms lst eq?))

(define (assoc-set lst itm val [is-equal? equal?])
  (define done #f)
  (define set (cons itm val))
  (define lst+
    (let loop ([lst lst])
      (cond
        [(null? lst) lst]
        [(is-equal? (caar lst) itm)
         (set! done #t)
         (cons set (cdr lst))]
        [else (cons (car lst) (loop (cdr lst)))])))
  (if done lst+
      (cons set lst)))
(define (assq-set lst itm val) (assoc-set lst itm val eq?))

(define eq-properties (make-weak-hasheq))

(define (eq-put! node property value)
  (define lst (assq-set (hash-ref eq-properties node '())
                        property value))
  (hash-set! eq-properties node lst)
  node)

(define (eq-get node property)
  (let ((plist (hash-ref eq-properties node '())))
    (let ((vcell (assq property plist)))
      (if vcell
	  (cdr vcell)
	  #f))))

(define (eq-rem! node . properties)
  (define lst (assq-del* properties
                          (hash-ref eq-properties node '())))
  (hash-set! eq-properties node lst)
  node)


(define (eq-adjoin! node property new)
  (eq-put! node property
	   (lset-adjoin eq?
			(or (eq-get node property) '())
			new))
  node)

(define (eq-delete! node property obj)
  (eq-put! node property
           (remq* (list obj) (or (eq-get node property) '()))))


(define (eq-plist node)
  (let ((plist (hash-ref eq-properties node #f)))
    (if plist (cons node plist) #f)))

(define (eq-clone! source target)
  (hash-set! eq-properties target
    (hash-ref eq-properties source '()))
  target)

(define (eq-label! node . plist)
  (let loop ((plist plist))
    (cond ((null? plist) node)
	  ((null? (cdr plist)) (error "Malformed plist"))
	  (else
	   (eq-put! node (car plist) (cadr plist))
	   (loop (cddr plist))))))

;;; Path names are built with properties.

(define (eq-path path)
  (define (lp node)
    (if node
	(if (pair? path)
	    (eq-get ((eq-path (cdr path)) node)
		    (car path))
	    node)
	#f))
  lp)
