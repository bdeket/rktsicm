#lang racket/base

(provide (all-defined-out))

(require "../../general/assert.rkt"
         "types.rkt"
         "generic.rkt"
         "../numeric.rkt")

;;;                Data Structure
;;; A differential quantity is a typed list of differential terms,
;;; representing the power series alluded to earlier.  The terms are
;;; kept in a sorted order, in ascending order. (Order is the number
;;; of incrementals.  So dx*dy is higher order than dx or dy.)

;;bdk;; insert 0 : from types
(define (differential? obj)
  (and (pair? obj)
       (eq? (car obj) differential-type-tag)))
;;bdk;; insert 0 end

;;bdk;; insert 1
(define (make-differential-quantity differential-term-list)
  (cons differential-type-tag differential-term-list))

(define (differential-term-list diff)
  (assert (differential? diff))
  (cdr diff))

(define (differential->terms diff)
  (cond ((differential? diff)
	 (filter (lambda (term)
		   (not (g:zero? (differential-coefficient term))))
		 (differential-term-list diff)))
	((g:zero? diff) '())
	(else (list (make-differential-term '() diff)))))

(define (terms->differential terms)
  (cond ((null? terms) :zero)
	((and (null? (cdr terms))
	      (null? (differential-tags (car terms))))
	 (differential-coefficient (car terms)))
	(else
	 (make-differential-quantity terms))))
;;bdk;; insert 1 end

;;; Each differential term has a list of tags.  The tags represent the
;;; incrementals.  Roughly, "dx" and "dy" are tags in the terms: 3*dx,
;;; 4*dy, 2*dx*dy.  There is a tag created for each derivative that is
;;; in progress.  Since the only use of a tag is to distinguish
;;; unnamed incrementals we use positive integers for the tags.

;;bdk;; insert 2
(define (make-differential-term tags coefficient)
  (list tags coefficient))

(define (differential-tags dterm)
  (car dterm))

(define (differential-coefficient dterm)
  (cadr dterm))
;;bdk;; insert 2 end

;;bdk;; insert 3
(define (differential-of x)
  (let lp ((x x))
    (if (differential? x)
	(lp (differential-coefficient
	     (car (differential-term-list x))))
	x)))
;;bdk;; insert 3 end