#lang racket/base

(provide (except-out (all-defined-out) assoc-del assq-del assoc-del* assq-del* assoc-set assq-set))

(require (only-in "../rkt/glue.rkt" hash-table/get hash-table/put! delq)
         "list-utils.rkt")

;;bdk;; start original file

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

;;;; Traditional LISP property lists
;;; extended to work on any kind of eq? data structure.

;;; Property lists are a way of creating data that looks
;;; like a record structure without commiting to the fields
;;; that will be used until run time.  The use of such
;;; flexible structures is frowned upon by most computer
;;; scientists, because it is hard to statically determine
;;; the bounds of the behavior of a program written using
;;; this stuff.  But it makes it easy to write programs that
;;; confuse such computer scientists.  I personally find it
;;; difficult to write without such crutches.  -- GJS

(define eq-properties (make-weak-hasheq))

(define (eq-put! node property value)
  (let ([plist (assq-set (hash-table/get eq-properties node '())
                         property value)])
    (hash-set! eq-properties node plist))
  #;
  (let ((plist (hash-table/get eq-properties node '())))
    (let ((vcell (assq property plist)))
      (if vcell
	  (set-cdr! vcell value)
	  (hash-table/put! eq-properties node
			   (cons (cons property value)
				 plist)))))
  node)

(define (eq-get node property)
  (let ((plist (hash-table/get eq-properties node '())))
    (let ((vcell (assq property plist)))
      (if vcell
	  (cdr vcell)
	  #f))))

(define (eq-rem! node . properties)
  (let ([plist (assq-del* properties
                          (hash-ref eq-properties node '()))])
    (hash-table/put! eq-properties node plist))
  #;
  (for-each
   (lambda (property)
     (let ((plist
	    (hash-table/get eq-properties node '())))
       (let ((vcell (assq property plist)))
	 (if vcell
	     (hash-table/put! eq-properties node
			      (delq! vcell plist))))))
   properties)
  node)

(define (eq-adjoin! node property new)
  (eq-put! node property
	   (lset-adjoin eq?
			(or (eq-get node property) '())
			new))
  node)

(define (eq-delete! node property obj)
  (eq-put! node property
           (delq obj (or (eq-get node property) '()))))


(define (eq-plist node)
  (let ((plist (hash-table/get eq-properties node #f)))
    (if plist (cons node plist) #f)))

(define (eq-clone! source target)
  (hash-table/put! eq-properties target
    (hash-table/get eq-properties source '()))
  #;
  (let ((plist (hash-table/get eq-properties source #f)))
    (if plist
        (hash-table/put! eq-properties target plist)))
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
