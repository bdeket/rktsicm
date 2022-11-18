#lang racket/base

(provide (all-defined-out))

(require "../../rkt/fixnum.rkt"
         "../../kernel-intr.rkt"
         "../../general/assert.rkt")

;;;;      Moments of a Distribution

;;;   Probably written by Matthew Halfant.
;;;   Modified by GJS on 17 December 2012.

;;; First some specific moments of a vector

(define (v:mean v)
  (let ((n (vector-length v)))
    (assert (not (zero? n))
	    "Need some data -- v:mean")
    (let lp ((i 0) (sum 0))
      (if (fix:= i n)
	  (/ sum n)
	  (lp (fix:+ i 1)
	      (+ sum (vector-ref v i)))))))

;;; Watch out! The following program incorporates
;;;  numerical-analysis magic.  The value of SUM
;;;  may appear to be zero, but see equation 1.7 
;;;  in Chan, Golub, and LeVeque; TR#222, Yale 
;;;  University CS Dept. 1983.

;;; The default here is "Population variance", but if 
;;; SAMPLE? is not false we compute "Sample variance".
;;; My HP calculator computes the latter--GJS. 

(define (v:variance-helper v sample?)
  (define (square x) (* x x))
  (let ((n (vector-length v))
	(mean (v:mean v)))
    (assert (not (and (one? n) sample?))
	    "Need more data -- v:sample-variance")
    (let lp ((i 0) (sumsq 0) (sum 0))
      (if (fix:= i n)
	  (/ (- sumsq (/ (square sum) n))
	     (if (not sample?)
		 n
		 (fix:- n 1)))
	  (let ((y (- (vector-ref v i) mean)))
	    (lp (fix:+ i 1)
		(+ sumsq (square y))
		(+ sum y)))))))
    
(define (v:variance v)
  (v:variance-helper v #f))
    
(define (v:sample-variance v)
  (v:variance-helper v #t))

(define (v:standard-deviation v)
  (sqrt (v:variance-helper v #f)))

(define (v:sample-standard-deviation v)
  (sqrt (v:variance-helper v #t)))

(define (v:average-deviation v)
  (let ((n (vector-length v))
	(mean (v:mean v)))
    (let lp ((i 0) (sum 0))
      (if (fix:= i n)
	  (/ sum n)
	  (let ((y (abs (- (vector-ref v i) mean))))
	    (lp (fix:+ i 1)
		(+ sum y)))))))

;;; We can calculate them all at once

(define (v:moments-helper v sample? cont)
  ;; cont =  (lambda (mean var std skew kurt adev) ...)
  (define (square x) (* x x))
  (let ((n (vector-length v))
	(mean (v:mean v)))
    (assert (not (and (one? n) sample?))
	    "Need more data -- sample-variance")
    (let lp ((i 0) (sum 0) (sumsq 0) (sumcb 0) (sumqu 0) (asum 0))
      (if (fix:= i n)
	  (let* ((var (/ (- sumsq (/ (square sum) n))
			 (if (not sample?)
			     n
			     (fix:- n 1))))
		 (std (sqrt var))
		 (skew (/ sumcb (* n var std)))
		 (kurt (- (/ sumqu (* n (square var))) 3.0))
		 (adev (/ asum n)))
	    (cont mean var std skew kurt adev))
	  (let* ((y (- (vector-ref v i) mean))
		 (yy (* y y))
		 (yyy (* y yy))
		 (yyyy (* y yyy)))
	    (lp (fix:+ i 1)
		(+ sum y)
		(+ sumsq yy)
		(+ sumcb yyy)
		(+ sumqu yyyy)
		(+ asum (abs y))))))))

;;; For lists

(define (mean l)
  (v:moments-helper (list->vector l)
		    #f
		    (lambda (mean var std skew kurt adev)
		      mean)))

(define (variance l)
  (v:moments-helper (list->vector l)
		    #f
		    (lambda (mean var std skew kurt adev)
		      var)))

(define (standard-deviation l)
  (v:moments-helper (list->vector l)
		    #f
		    (lambda (mean var std skew kurt adev)
		      std)))

(define (sample-variance l)
  (v:moments-helper (list->vector l)
		    #t
		    (lambda (mean var std skew kurt adev)
		      var)))

(define (sample-standard-deviation l)
  (v:moments-helper (list->vector l)
		    #t
		    (lambda (mean var std skew kurt adev)
		      std)))

(define (skewness l)
  (v:moments-helper (list->vector l)
		    #f
		    (lambda (mean var std skew kurt adev)
		      skew)))

(define (kurtosis l)
  (v:moments-helper (list->vector l)
		    #f
		    (lambda (mean var std skew kurt adev)
		      kurt)))

(define (average-deviation l)
  (v:moments-helper (list->vector l)
		    #f
		    (lambda (mean var std skew kurt adev)
		      adev)))

;;; Streams of data have running moments

(define (running-mean decay stream)
  (let loop ((sum (stream-first stream))
	     (count 1)
	     (stream (stream-rest stream)))
    (stream-cons (/ sum count)
		 (loop (+ (stream-first stream) (* decay sum))
		       (+ 1 (* decay count))
		       (stream-rest stream)))))

