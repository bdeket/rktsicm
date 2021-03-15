#lang racket/base

(provide (all-defined-out))

(require racket/fixnum
         "../../kernel-intr.rkt")

;;; UNIFORM-RANDOM produces an inexact number x,    0 <= x < 1

(define (uniform-random) (random 1.))

(define (nonzero-uniform-random)
  (let ((x (uniform-random)))
    (if (= x 0.)
	(nonzero-uniform-random)
	x)))

;;; Given uniform random numbers, we can produce pairs of
;;; gaussian-distributed numbers, with zero mean and unit
;;; standard deviation, by the following trick:

(define (gaussian-random-pair [continue cons])
  ;; continue = (lambda (y1 y2) ...)
  (let ((x1 (uniform-random))
	(x2 (uniform-random)))
    (let ((r (sqrt (* -2.0 (log x1)))))
      (continue (* r (cos (* 2pi x2)))
		(* r (sin (* 2pi x2)))))))

(define (gaussian-random)
  (gaussian-random-pair (lambda (x y) x)))

(define (gaussian-random-list d)
  (let lp ((j d) (t '()))
    (if (fx= j 0)
	t
	(gaussian-random-pair
	 (lambda (x1 x2)
	   (if (fx= j 1)
	       (cons x1 t)
	       (lp (fx- j 2) (cons x1 (cons x2 t)))))))))


;;; Makes a list of n 2-vectors of gaussian-distributed random numbers  

(define (gaussian-random-pairs n)
  (if (fx= n 0) 
      '()
      (cons (gaussian-random-pair vector)
	    (gaussian-random-pairs (fx- n 1)))))

;;; Makes a list of n d-vectors of gaussian-distributed random numbers  

(define (gaussian-random-tuples d n)
  (if (fx= n 0) 
      '()
      (cons (list->vector (gaussian-random-list d))
	    (gaussian-random-tuples d (fx- n 1)))))

;;; For adding zero-mean noise with a given standard deviation to a vector.

(define ((add-noise sigma) v)
  (list->vector (map (lambda (signal noise)
		       (+ signal (* sigma noise)))
		     (vector->list v)
		     (gaussian-random-list (vector-length v)))))
