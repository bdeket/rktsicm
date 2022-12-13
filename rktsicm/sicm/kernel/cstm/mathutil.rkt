#lang racket/base

(provide (all-defined-out))

(require (only-in "../../rkt/glue.rkt" if subvector
                  fix:< fix:+)
         "../../general/list-utils.rkt"
         "../utils.rkt"
         "s-operator.rkt"
         "matrices.rkt"
         "pseries.rkt"
         "structs.rkt"
         "../types.rkt"
         "../strutl.rkt"
         )

;;bdk;; insert 1
(define (g:ref x . selectors)
  (ref-internal x selectors))
;;bdk;; insert 1 end

;;bdk;; insert 2
(define (ref-internal x selectors)
  (cond ((null? selectors) x)
	((procedure? x)
	 (if (operator? x)
	     (make-operator (compose (lambda (y)
				       (ref-internal y selectors))
				     x)
			    `(compose (component ,@selectors)
				      ,(operator-name x))
			    (operator-subtype x))
	     (compose (lambda (y)
			(ref-internal y selectors))
		      x)))
	(else
	 (let ((i (car selectors)) (js (cdr selectors)))
	   (cond ((exact-integer? i)
		  (cond ((vector? x)
			 (ref-internal
			  (vector-ref x (adjust-index i (vector-length x)))
			  js))
			((structure? x)
			 (ref-internal (s:ref x (adjust-index i (s:length x)))
				       js))
			((matrix? x)
			 (if (null? js)
			     (cond ((column-matrix? x)
				    (ref-internal
				     (matrix-ref x
						 (adjust-index i (m:num-rows x))
						 0)
				     js))
				   ((row-matrix? x)
				    (ref-internal
				     (matrix-ref x
						 0
						 (adjust-index i
							       (m:num-cols x)))
				     js))
				   (else
				    (error "Not enuf indices -- REF" x i js)))
			     (ref-internal
			      (matrix-ref x 
					  (adjust-index i (m:num-rows x))
					  (adjust-index (car js)
							(m:num-cols x)))
			      (cdr js))))
			((series? x)
			 (ref-internal (stream-ref (series->stream x) i) js))
			((stream-pair? x)
			 (ref-internal (stream-ref x i) js))
			((list? x)
			 (ref-internal
			  (list-ref x (adjust-index i (length x)))
			  js))
			((string? x)
			 (if (not (null? js))
			     (error "String has no substructure -- REF" x i js))
			 (string-ref x (adjust-index i (string-length x))))
			(else
			 (error "Unknown compound -- G:REF" x i))))
		 ((and (pair? i)
		       (exact-integer? (car i))
		       (pair? (cdr i))
		       (exact-integer? (cadr i)))
		  (cond ((vector? x)
			 (ref-internal
                          (subvector x
                                     (adjust-index (car i) (vector-length x))
                                     (adjust-end (cadr i) (vector-length x)))
                          js))
                        ((structure? x)
                         (ref-internal
                          (s:structure
                           (s:type x)
                           (subvector (s:->vector x)
                                      (adjust-index (car i) (s:length x))
                                      (adjust-end (cadr i) (s:length x))))
			  js))
			((matrix? x)
			 (if (null? js)
			     (cond ((column-matrix? x)
				    (ref-internal
				     (m:submatrix x
						  (adjust-index (car i)
								(m:num-rows x))
						  (adjust-end (cadr i)
							      (m:num-rows x))
						  0
						  1)
				     js))
				   ((row-matrix? x)
				    (ref-internal
				     (m:submatrix x
						  0
						  1
						  (adjust-index (car i)
								(m:num-cols x))
						  (adjust-end (cadr i)
							      (m:num-cols x)))
				     js))
				   (else
				    (error "Not enuf indices -- REF" x i js)))
			     (ref-internal
			      (m:submatrix x 
					   (adjust-index (car i) (m:num-rows x))
					   (adjust-end (cadr i) (m:num-rows x))
					   (adjust-index (caar js)
							 (m:num-cols x))
					   (adjust-end (cadar js)
						       (m:num-cols x)))
			      (cdr js))))
			((list? x)
			 (ref-internal
			  (sublist x
				   (adjust-index (car i) (length x))
				   (adjust-end (cadr i) (length x)))
			  js))
			((string? x)
			 (if (not (null? js))
			     (error "String has no substructure -- REF" x i js))
			 (substring x
				    (adjust-index (car i) (string-length x))
				    (adjust-end (cadr i) (string-length x))))
			(else
			 (error "Unknown compound -- G:REF" x i))))
		 (else
		  (error "Unknown selector type -- REF" x i js)))))))

(define (adjust-index i n)
  (if (fix:< i 0)
      (let ((j (fix:+ n i)))
	(if (fix:< j 0)
	    (error "Bad index -- REF" i))
	j)
      (begin
	(if (not (fix:< i n))
	    (error "Bad index -- REF" i))
	i)))

(define (adjust-end i n)
  (let ((n (fix:+ n 1)))
    (if (fix:< i 0)
	(let ((j (fix:+ n i)))
	  (if (fix:< j 0)
	      (error "Bad index -- REF" i))
	  j)
	(begin
	  (if (not (fix:< i n))
	      (error "Bad index -- REF" i))
	  i))))
;;bdk;; insert 2 end
