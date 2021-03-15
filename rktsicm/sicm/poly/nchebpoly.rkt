#lang racket/base

(provide (all-defined-out))

(require racket/fixnum
         "../kernel-gnrc.rkt"
         "../simplify.rkt"
         "domain.rkt"
         )

;;;; Chebyshev polynomial routines

;;;  Must be loaded into an environment where polynomial manipulations
;;;  exist.

;;; Edited by GJS 10Jan09

;;; 1/24/90 (gjs) converted to flush dependencies on dense list
;;; representation.

;;; 9/22/89 (gjs) reduce->a-reduce

;;; 12/23/87 (mh) modified CHEB-ECON to return original poly if not
;;; trimmed

(define (add-lists l1 l2)
  (cond ((null? l1) l2)
	((null? l2) l1)
	(else
	 (cons (+ (car l1) (car l2))
	       (add-lists (cdr l1) (cdr l2))))))

(define (scale-list s l)
  (map (lambda (x) (* s x)) l))

;;; We define the stream of Chebyshev polynomials,
;;;  using the recurrence relation
;;;           T[0] = 1, T[1] = x, T[n] = 2xT[n-1] - T[n-2]

(define chebyshev-polynomials
  (let ((x poly:identity)
	(2x (poly:scale poly:identity 2)))
    (stream-cons 1
		 (stream-cons x
			      (map-streams (lambda (p1 p2)
					     (poly:- (poly:* 2x p1) p2))
					   (stream-rest chebyshev-polynomials)
					   chebyshev-polynomials)))))


;;; The following procedure returns the Nth Chebyshev polynomial

(define (chebyshev-polynomial n)
  (stream-ref chebyshev-polynomials n))


;;; In the following, we define a CHEB-EXP to be an expansion in
;;;  Chebyshev polynomials.  The expansion is
;;;  written as a list of coefficients (a0 a1 ... aN), and represents
;;;  the formal series a0*T[0](x) + ... + aN*T[N](x).

;;; The stream of scaled Chebyshev expansions corresponding
;;;  to the sequence 1, x, 2x^2, 4x^3, ..., 2^(n-1)x^n ... Note that the
;;;  general form doesn't cover the first term.  What is generally wanted
;;;  is the expansion corresponding to x^n; the power of 2 is thrown in
;;;  so that the resulting expansion is exact in integer arithmetic.

(define (2x cheb-exp)
    (let ((t1 (cdr cheb-exp))
          (t2 (append (list 0) cheb-exp))
          (t3 (list 0 (car cheb-exp))))
      (add-lists t1 (add-lists t2 t3))))

(define scaled-chebyshev-expansions
  (stream-cons '(1)
   (stream-cons '(0 1)
    (map-stream 2x
		(stream-rest scaled-chebyshev-expansions)))))


;;; For convenience, we also provide the non-scaled Chebyshev expansions

(define chebyshev-expansions
  (letrec (;; s = {1 1 2 4 8 16 ...}
	   (s (stream-cons 1
	       (stream-cons 1
		(map-stream (lambda (x) (+ x x)) (stream-rest s)))))
           (c scaled-chebyshev-expansions))
    (map-streams (lambda (factor expansion)
                   (scale-list (/ 1 factor) expansion))
                 s
                 c)))


;;; Convert from polynomial form to Chebyshev expansion

(define (poly->cheb-exp poly)
  (let* ((maxcoeff (apply max (map abs (poly/coefficients poly))))
	 (zero-tolerance (* 10 maxcoeff *machine-epsilon*))
	 (=0?
	  (lambda (p)
	    (and (number? p)
		 (< (abs p) zero-tolerance)))))
    (let lp ((p poly) (c chebyshev-expansions) (s '(0)))
      (if (=0? p)			;(equal? p poly:zero) NO!
	  s
	  (let ((v (poly:value p 0)))
	    (poly:divide (poly:- p v) poly:identity
			 (lambda (q r)
			   (when (not (equal? r poly:zero))
			       (error "POLY->CHEB-EXP"))
			   (lp q
			       (stream-rest c)
			       (add-lists (scale-list v (stream-first c)) s)))))))))


;;; Convert from Chebyshev expansion to polynomial form

(define (cheb-exp->poly e)
  (let ((n (length e)))
    (let ((cheb (stream-head chebyshev-polynomials n)))
      (a-reduce poly:+ (map poly:scale cheb e)))))


;;; Given a Cheb-expansion and an error criterion EPS, trim the tail of
;;;  those coefficients whose absolute sum is <= EPS.

(define (trim-cheb-exp cheb eps)
  (let ((r (reverse cheb)))
    (let loop ((sum (abs (car r))) (r r))
      (if (fx= (length r) 1)
          (if (<= sum eps) '(0) r)
          (if (> sum eps)
              (reverse r)
              (loop (+ sum (abs (cadr r))) (cdr r)))))))


;;; The next procedure performs Chebyshev economization on a
;;;  polynomial p over a specified interval [a,b]: the returned
;;;  polynomial is guaranteed to differ from the original by no more
;;;  than eps over [a, b].

(define (cheb-econ p a b eps)
  (let ((q (poly-domain->canonical p a b)))
    (let ((r (poly->cheb-exp q)))
      (let ((s (trim-cheb-exp r eps)))
        (if (fx= (length s) (length r)) ;nothing got trimmed
            p
            (let ((t (cheb-exp->poly s)))
              (poly-domain->general t a b)))))))

;;; Return the root-list of the Nth Chebyshev polynomial

(define (cheb-root-list n)
  (define (root i)
    (if (and (odd? n)
	     (fx= (fx* 2 i) (fx- n 1)))
        0
        (- (cos (/ (* (+ i 1/2) pi) n)))))
  (let loop ((i 0))
    (if (fx= i n)
        '()
        (cons (root i)
              (loop (fx+ i 1))))))

;;; This procedure accepts an integer n > 0 and a real x, and returns
;;; a list of the values T[0](x) ... T[n-1](x). If an optional third
;;; argument is given as the symbol 'HALF, then the first value in the
;;; list will have the value 0.5; otherwise it has the value 1.

(define (first-n-cheb-values n x . optionals)
  (let ((first (if (and (not (null? optionals))
                        (eq? (car optionals) 'HALF))
                   1/2
                   1)))
    (cond ((fx< n 1) '())
          ((fx= n 1) (list first))
          ((fx= n 2) (list first x))
          (else (let loop ((ans (list x first)) (a 1) (b x) (count 2))
                  (if (fx= count n)
                      (reverse ans)
                      (let ((next (- (* 2 x b) a)))
                        (loop (cons next ans) b next (fx+ count 1)))))))))


;;; The following procedure evaluates a cheb-expansion directly, in a
;;;  manner analogous to Horner's method for evaluating polynomials --
;;;  actually, it isn't as analogous as it should be, and should
;;;  probably be replaced by Clenshaw's method which truly is like
;;;  Horner's.

(define (cheb-exp-value cheb x)
  (let ((n (length cheb)))
    (let ((vals (first-n-cheb-values n x)))
      (a-reduce + (map * cheb vals)))))


;;; This procedure generates a Chebyshev expansion to a given order N,
;;;  for a function f specified on an interval [a,b]. The interval is
;;;  mapped onto [-1,1] and the function approximated is g, defined on
;;;  [-1,1] to behave the same as f on [a,b].

;;; Note: the returned list of coefficients is N long, and is
;;; associated with Chebyshev polynomials from T[0] to T[N-1].

(define (generate-cheb-exp f a b n)
  (when (<= b a)
      (error "Bad interval in GENERATE-CHEB-EXP"))
  (let ((interval-map    ;map [-1,1] onto [a,b]
          (let ((c (/ (+ a b) 2))
                (d (/ (- b a) 2)))
            (lambda (x) (+ c (* d x))))))
    (let ((roots (cheb-root-list n))
          (polys (stream-head chebyshev-polynomials n)))
      (let ((vals (map f (map interval-map roots))))
        (let loop ((coeffs '()) (i 0))
          (if (fx= i n)
              (reverse coeffs)
              (let ((chebf (lambda (x)
			     (poly:value (list-ref polys i) x))))
                (let ((chebvals (map chebf roots)))
                  (let ((sum (a-reduce + (map * vals chebvals))))
                    (let ((term (if (zero? i) (/ sum n) (/ (* 2 sum) n))))
                      (loop (cons term coeffs) (fx+ i 1))))))))))))


;;; This procedure accepts a function f, an interval [a,b], a number N
;;; of points at which to base a Chebyshev interpolation, and an
;;; optional precision EPS. The method is to map f onto the canonical
;;; interval [-1,1], generate the Chebyshev expansion based on the
;;; first N Chebyshev polynomials interpolating at the roots of the
;;; N+1st Chebyshev polynomial T[N]. If EPS has been specified, an
;;; economization is performed at this point; otherwise not. Finally,
;;; the Chebyshev expansion is reconverted to a polynomial and mapped
;;; back onto the original interval [a,b].

(define (generate-approx-poly f a b n . optionals)
  (let ((eps (if (null? optionals) #f (car optionals))))
    (let ((p (generate-cheb-exp f a b n)))
      (let ((pp (if eps (trim-cheb-exp p eps) p)))
        (poly-domain->general (cheb-exp->poly pp) a b)))))

#|
(define win (frame 0 pi/2 0 1))

(define s
  (let ((p (generate-approx-poly sin 0 pi/2 3)))
    (lambda (x) (poly:value p x))))

(plot-function win sin 0 pi/2 .01)
(plot-function win s 0 pi/2 .01)

(graphics-close win)

(define win (frame 0 pi/2 -0.1 +0.1))

(plot-function win (- s sin) 0 pi/2 .01)

(graphics-close win)

(define win (frame 0 pi/2 -0.002 +0.002))

(define s1
  (let ((p (generate-approx-poly sin 0 pi/2 5)))
    (lambda (x) (poly:value p x))))

(plot-function win (- s1 sin) 0 pi/2 .01)

(define s2
  (let ((p (generate-approx-poly sin 0 pi/2 5 .01)))
    (lambda (x) (poly:value p x))))

(plot-function win (- s2 sin) 0 pi/2 .01)

(graphics-close win)
|#
