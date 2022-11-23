#lang racket/base

(provide make-table adjoin-to-list! put! table-of lookup)

(require (only-in "../rkt/glue.rkt" false)
         "sets.rkt")

;;;; Property Tables

;;; Properties are n-dimensional sparse tables implemented as 
;;; nests of association lists.

;;; For any given sequence of keys, there can be both a value
;;; and a subtable.  A table is a list of a value and some entries.
;;; An entry is a pair, whose CAR is a key and whose CDR is a
;;; the subtable for that key.

(define (make-table table-name assoc)
  (let ((local-table (list *no-value*)))

    (define (lookup keys)
      (define (loop keys table)
	(if (null? keys) (car table)
	    (let ((entry (assoc (car keys) (cdr table))))
	      (if entry
		  (loop (cdr keys) (cdr entry))
		  *no-value*))))
      (loop keys local-table))

    (define (smash! keys value)
      #;
      (define (loop keys table)
	(if (null? keys) (set-car! table value)
	    (let ((entry (assoc (car keys) (cdr table))))
	      (if entry
		  (loop (cdr keys) (cdr entry))
		  (set-cdr! table
			    (cons (cons (car keys)
					(make-subtable (cdr keys) value))
				  (cdr table)))))))
      (define (loop keys table)
	(if (null? keys)
            (cons value (cdr table))
	    (let ((entry (assoc (car keys) (cdr table))))
	      (if entry
		  (for/list ([i (in-list table)])
                    (if (eq? i entry)
                        (cons (car entry) (loop (cdr keys) (cdr entry)))
                        i))
                  (cons (car table)
                        (cons (cons (car keys)
                                    (make-subtable (cdr keys) value))
                              (cdr table)))))))
      (set! local-table (loop keys local-table))
      local-table)

    (define (make-subtable keys value)
      (if (null? keys) (list value)
	  (list *no-value*
		(cons (car keys)
		      (make-subtable (cdr keys) value)))))

    (define (accumulator! increment-procedure initial-value keys value)
      #;
      (define (loop keys table)
	(if (null? keys)
	    (if (eq? (car table) *no-value*)
		(set-car! table (increment-procedure value initial-value))
		(set-car! table (increment-procedure value (car table))))
	    (let ((entry (assoc (car keys) (cdr table))))
	      (if entry
		  (loop (cdr keys) (cdr entry))
		  (set-cdr! table
			    (cons (cons (car keys)
					(make-subtable (cdr keys)
						       (increment-procedure value
									    initial-value)))
				  (cdr table)))))))
      (define (loop keys table)
	(if (null? keys)
            (cons (increment-procedure value
                                       (if (eq? (car table) *no-value*)
                                           initial-value
                                           (car table)))
                  (cdr table))
	    (let ([entry (assoc (car keys) (cdr table))])
	      (if entry
                  (for/list ([i (in-list table)])
                    (if (eq? i entry)
                        (cons (car entry) (loop (cdr keys) (cdr entry)))
                        i))
                  (cons (car table)
                        (cons (cons (car keys)
                                    (make-subtable (cdr keys)
                                                   (increment-procedure value
                                                                        initial-value)))
                              (cdr table)))))))
      (set! local-table (loop keys local-table))
      local-table)

    (define (remove! keys) (smash! keys *no-value*))

    (define (show) local-table)

    (vector table-name lookup smash! accumulator! remove! show)))


(define *no-value* (list '*no-value*))

(define (no-value? value)
  (eq? value *no-value*))


(define (get table . keys)
  ((vector-ref table 1) keys))

(define ((getter table) . keys)
  ((vector-ref table 1) keys))


(define (put! table value . keys)
  ((vector-ref table 2) keys value)
  'done)

(define ((putter! table) value . keys)
  ((vector-ref table 2) keys value)
  'done)


(define (get-with-default table default . keys)
  (let ((v ((vector-ref table 1) keys)))
    (if (eq? v *no-value*)
	default
	v)))

(define ((getter-with-default table default) . keys)
  (let ((v ((vector-ref table 1) keys)))
    (if (eq? v *no-value*)
	default
	v)))


(define (get-with-check table . keys)
  (let ((v ((vector-ref table 1) keys)))
    (if (eq? v *no-value*)
	(error "can't find value in table"
	       (list table keys))
	v)))

(define ((getter-with-check table) . keys)
  (let ((v ((vector-ref table 1) keys)))
    (if (eq? v *no-value*)
	(error "can't find value in table"
	       (list table keys))
	v)))


(define (add-to-list! object table . keys)
  ((vector-ref table 3) cons '() keys object)
  'done)

(define (adjoin-to-list! object table . keys)
  ((vector-ref table 3) list-adjoin '() keys object)
  'done)

(define (store! object table . keys)
  ((vector-ref table 2) keys object)
  'done)

;;; Elementary table utilities implemented in ALISTs

(define (lookup key table)
  (let ((val (assq key table)))
    (if val
	(cadr val)
	(error "key not in table -- LOOKUP" key))))

(define (rlookup key table)
  (cond ((null? table) false)
	((null? (cdar table)) (rlookup key (cdr table)))
	((eq? key (cadar table)) (car table))
	(else (rlookup key (cdr table)))))

(define (rassq key table)
  (cond ((null? table) false)
	((eq? key (cdar table)) (car table))
	(else (rassq key (cdr table)))))

(define (rassoc key table)
  (cond ((null? table) false)
	((equal? key (cdar table)) (car table))
	(else (rassoc key (cdr table)))))

(define (disassoc key alist)
  (cond ((null? alist) '())
	((equal? key (caar alist))
	 (cdr alist))
	(else
	 (cons (car alist)
	       (disassoc key (cdr alist))))))


;;; Elementary table utility implemented as PLISTs

(define (default-lookup name default list)
  (let ((L (memq name list)))
    (if L (cadr L) default)))

(define (table-of is? keys values)
  (define (lookup key)
    (let next ((ks keys) (vs values))
      (cond ((null? ks)
             (error "Key not in table" key))
            ((is? key (car ks)) (car vs))
            (else (next (cdr ks) (cdr vs))))))
  lookup)
