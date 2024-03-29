#lang racket/base

(provide (all-defined-out))

(require "../rkt/fixnum.rkt"
         "../rkt/if.rkt"
         (only-in "../rkt/todo.rkt" todo set-car! set-cdr!))

(todo clean-expression-table "canonicalizer")

(define gc-reclaimed-object (gensym))
(define (gc-reclaimed-object? v) (eq? gc-reclaimed-object v))

(struct weak-pair (car cdr) #:transparent #:mutable)
(define (weak-cons car cdr) (weak-pair (make-weak-box car) cdr))
(define (weak-car W) (weak-box-value (weak-pair-car W) gc-reclaimed-object))
(define (weak-cdr W) (weak-pair-cdr W))
(define (weak-set-cdr! W v) (set-weak-pair-cdr! W v))
(define (weak-pair/car? W)
  (not (gc-reclaimed-object? (weak-car W))))

(define (list->weak-list lst)
  (if (pair? lst)
      (weak-cons (car lst) (list->weak-list (cdr lst)))
      lst))

;;bdk;; start original file

;;;; Weak list utilities


;;; Looks for obj in a weak list.

(define (get-weak-member obj weak-list)
  (if (null? weak-list)
      #f
      (let ((a (weak-car weak-list)))
	(if (equal? obj a)
	    a
	    (get-weak-member obj (weak-cdr weak-list))))))


;;; Looks for obj as the key in a weak alist.
;;;  The weak alist has a backbone that is a strong list
;;;    with weak pair entries.

(define (weak-find obj weak-alist)
  (if (null? weak-alist)
      #f
      (let ((pair (car weak-alist)))
	(if pair                        ;not dead pair
	    (let ((a (weak-car pair)))
	      (if (gc-reclaimed-object? a)
		  (begin (set-car! weak-alist #f)
			 #f)            ;kill this pair
		  (if (equal? obj a)
		      a
		      (weak-find obj
                                 (cdr weak-alist)))))
	    (weak-find obj (cdr weak-alist))))))

(define (weak-length weak-list)
  (if (weak-pair? weak-list)
      (fix:+ (weak-length (weak-cdr weak-list)) 1)
      0))

;;; Weak-alist searches.  These scan a weak alist for an object,
;;; returning the associated value if found.  They also clean up the
;;; alist by clobbering out value cells that have lost their key.
;;; These also work for strong alists, but strong alists are not
;;; modified.

(define (weak-finder same?)
  (define (the-finder obj weak-alist)
    (if (null? weak-alist)
	#f
	(let ((pair (car weak-alist)))
	  (cond ((weak-pair? pair)
		 (let ((a (weak-car pair)))
		   (if (gc-reclaimed-object? a)
		       (begin (set-car! weak-alist #f)
			      #f)
                       (if (same? obj a)
			   (weak-cdr pair)
			   (the-finder obj
                                       (cdr weak-alist))))))
		((pair? pair)
		 (let ((a (car pair)))
		   (if (same? obj a)
		       (cdr pair)
		       (the-finder obj (cdr weak-alist)))))
		(else
		 (the-finder obj (cdr weak-alist)))))))
  the-finder)

(define weak-find-equal? (weak-finder equal?))
(define weak-find-eqv? (weak-finder eqv?))
(define weak-find-eq? (weak-finder eq?))

;;; The following clips out dead linkages that have been clobbered by
;;; a weak finder (above).  It also limits the size of the alist to
;;; the maximum size specified, by chopping off the tail.  max-size
;;; must be a positive integer larger than 1.

(define (purge-list lst max-size)
  (cond
    [(null? lst) lst]
    [(= 0 max-size) '()]
    [(weak-car (car lst))
     (cons (car lst) (purge-list (cdr lst) (- max-size 1)))]
    [else (purge-list (cdr lst) max-size)]))

;;; Weak list cleanups

(define (clean-weak-list weak-list)
  (let clean-head ((this weak-list))
    (if (weak-pair? this)
	(let ((next (weak-cdr this)))
	  (if (gc-reclaimed-object? (weak-car this))
	      (clean-head next)
	      (begin
		(let clean-tail ((this next) (prev this))
		  (if (weak-pair? this)
		      (let ((next (weak-cdr this)))
			(if (gc-reclaimed-object? (weak-car this))
			    (begin
			      (weak-set-cdr! prev next)
			      (clean-tail next prev))
			    (clean-tail next this)))))
		this)))
	this)))

(define (clean-weak-alist weak-alist)
  (clean-alist weak-alist
	       (lambda (p)
		 (if (not (weak-pair? p))
		     (raise-argument-error 'clean-weak-alist "weak-alist" weak-alist))
		 (not (gc-reclaimed-object? (weak-car p))))))

(define (clean-subtable-alist alist)
  (clean-alist alist
               (lambda (p)
                 (if (not (pair? p))
                   (raise-argument-error 'clean-subtable-alist "weak-alist" alist))
                 (clean-expression-table (cdr p)))))

(define (clean-alist alist clean-association)
  (let clean-head ((this alist))
    (if (pair? this)
	(let ((next (cdr this)))
	  (if (clean-association (car this))
	      (begin
		(let clean-tail ((this next) (prev this))
		  (if (pair? this)
		      (let ((next (cdr this)))
			(if (clean-association (car this))
			    (clean-tail next this)
			    (begin
			      (set-cdr! prev next)
			      (clean-tail next prev))))))
		this)
	      (clean-head next)))
	this)))
