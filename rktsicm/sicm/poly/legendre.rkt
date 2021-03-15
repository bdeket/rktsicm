#lang racket/base

(provide (all-defined-out))

(require "../kernel-gnrc.rkt"
         "../simplify.rkt"
         )

;;; LEGENDRE.SCM -- the Legendre Polynomials returned as a stream or singly

;;; Edited by GJS 10Jan09

;;; The following defines a stream whose nth term (0-based) is
;;; P[n]. We use the recurrence relation:
;;;      P[0](x) = 1, P[1](x) = x
;;;   and for n > 1
;;;      P[n](x) = ((2n-1)/n)*x*P[n-1](x) - ((n-1)/n)*P[n-2](x)

(define legendre-polynomials
  (stream-cons poly:one
    (stream-cons poly:identity
      (map-streams (lambda (p1 p2)
		     (let* ((n (+ (poly:degree p1) 1))
			    (a (/ (- (* 2 n) 1) n))
			    (b (/ (- n 1) n)))
		       (poly:- (poly:* (poly:scale poly:identity a) p1)
			       (poly:scale p2 b))))
		   (stream-rest legendre-polynomials)
		   legendre-polynomials))))

(define (legendre-polynomial n)
  (stream-ref legendre-polynomials n))
