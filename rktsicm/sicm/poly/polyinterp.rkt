#lang racket/base

(provide (all-defined-out))

(require (only-in "../rkt/glue.rkt" if)
         "../general/assert.rkt"
         "../kernel-gnrc.rkt"
         "../simplify/pcf.rkt"
         "nchebpoly.rkt"
         "../numerics/optimize/unimin.rkt"
         )


;;bdk;; start original file

;;;; Numerical construction of Polynomial interpolations

;;; Edited by GJS 10Jan09


;;bdk;; moved to domain 1


;;; Given a list of distinct abscissas xs = (x1 x2 ... xn) and a list
;;; of ordinates ys = (y1 y2 ... yn), return the Lagrange interpolation
;;; polynomial through the points (x1, y1), (x2, y2), ... (xn, yn).

(define (make-interp-poly ys xs)
  ;; given a point list, return a poly that evaluates to 1 at the 
  ;; first point and 0 at the others.
  (define (roots->poly roots)
    (a-reduce poly:*
	      (map (lambda (r) (poly:- poly:identity r))
		   roots)))
  (define (unity-at-first-point point-list)
    (let* ((x (car point-list))
           (px (apply * (map (lambda (u) (- x u)) 
			     (cdr point-list)))))
      (if (zero? px)
          (error "MAKE-INTERP-POLY: abscissas not distinct"))
          (poly:scale (roots->poly (cdr point-list)) (/ 1 px))))
  (let loop ((p poly:zero) (points xs) (values ys))
    (if (null? values)
        p
        (let ((q (unity-at-first-point points)))
          (loop (poly:+ p (poly:scale q (car values)))
                (left-circular-shift points)
                (cdr values))))))

;;; Given a function F, an interval [a, b], and a specified N > 0: we 
;;; generate a polynomial P of order N (and degree N-1) that interpolates 
;;; F at the "Chebyshev" points mapped onto [a, b].  We assume that the 
;;; absolute error function E(x) = |F(x) - P(x)| is unimodal between
;;; adjacent interpolation points.  Thus E(x) has altogether N+1 "bumps"; 
;;; the largest of these has height Bmax and the smallest has height Bmin. 
;;; Of course Bmax is an error bound for the approximation of F by P on 
;;; [a, b]; but Bmin has heuristic significance as a lower-bound for the 
;;; reduced error that might be obtained by tuning the interpolation points 
;;; to meet the equiripple criterion. We return the list (P Bmax Bmin).

(define (get-poly-and-errors f a b n)
  (let* ((c (/ (+ a b) 2))
         (d (/ (- b a) 2))
         (imap (lambda (x) (+ c (* d x)))) ;map [-1, 1] -> [a, b]
         (points (map imap (cheb-root-list n)))
         (p (make-interp-poly (map f points) points))
         (abserr (lambda (x) (abs (- (f x) (poly:value p x)))))
         (abserr-a (abserr a))
         (abserr-b (abserr b))
         (max-and-min-bumps
           (let loop ((pts points)
                      (bmax (max abserr-a abserr-b))
                      (bmin (min abserr-a abserr-b)))
             (if (< (length pts) 2)
                 (list bmax bmin)
                 (let ((x0 (car pts)) (x1 (cadr pts)))
                   (let ((bump (cadr (brent-max abserr x0 x1 1e-6))))
                     (cond ((> bump bmax) (loop (cdr pts) bump bmin))
                           ((< bump bmin) (loop (cdr pts) bmax bump))
                           (else (loop (cdr pts) bmax bmin)))))))))
    (list p (car max-and-min-bumps) (cadr max-and-min-bumps))))


;;; Often we want a function that computes the value of a polynomial
;;; at given points.

(define ((polynomial-function poly) x)
  (assert (pcf? poly) "Not a polynomial")
  (poly/horner-univariate poly x))

