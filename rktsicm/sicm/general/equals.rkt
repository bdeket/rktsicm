#lang racket/base

(provide (all-defined-out))

(require (only-in "../rkt/glue.rkt" number:eqv? fix:= fix:+))

;;bdk;; start original file


(define (simple:equal? x y)
  (or (eq? x y)
      (cond ((pair? x)
             (and (pair? y)
                  (simple:equal? (car x) (car y))
                  (simple:equal? (cdr x) (cdr y))))
            ((vector? x)
             (and (vector? y)
                  (vector:equal? x y)))
            ((number? x)
             (and (number? y)
                  (number:eqv? x y)))
            ((string? x)
             (and (string? y)
                  (string=? x y)))
            (else #f))))

(define (vector:equal? x y)
  (let ((size (vector-length x)))
    (and (fix:= size (vector-length y))
	 (let loop ((index 0))
	   (or (fix:= index size)
	       (and (simple:equal? (vector-ref x index)
				   (vector-ref y index))
		    (loop (fix:+ index 1))))))))

(define (pair:eq? x y)
  (and (eq? (car x) (car y))
       (eq? (cdr x) (cdr y))))
  

;;; Problem in simplify/simplify.scm there is a hash
;;; table that I don't know how to handle.