#lang racket/base

(provide (all-defined-out)
         (all-from-out "cstm/numeric.rkt")
         conjugate)

(require "../rkt/fixnum.rkt"
         (only-in racket/math conjugate)
         "cstm/numeric.rkt"
         "../general/assert.rkt"
         "../general/memoize.rkt"
         "../general/permute.rkt"
         "../rkt/default-object.rkt"
         "../rkt/int.rkt"
         (only-in "../rkt/todo.rkt" access ->environment)
         )

;;;; Extensions to Scheme numbers
;;; (ulp x) is the distance to the next represented floating-point
;;;  number after x.

(define (ulp x)
 (let ((x (abs (exact->inexact x))))
   (- (* x (+ 1 *machine-epsilon*)) x)))

;;; (ulp 1.0) = *machine-epsilon*
;;; (ulp 2.0) = 2*(ulp 1.0)
;;; (ulp 3.0) = 4*(ulp 1.0)
;;; (ulp 4.0) = 4*(ulp 1.0)
;;; (ulp 5.0) = 4*(ulp 1.0)
;;; (ulp 6.0) = 8*(ulp 1.0)

;;; (ulpr x) is an alternate measure.

(define (ulpr x)
  (let ((x (exact->inexact x)))
    (abs (/ (* x *machine-epsilon*) 2))))

;;; (ulpr 1.0) = *machine-epsilon*/2

(define (exact-rational? x)
  (and (real? x) (exact? x)))

(define (exact-zero? x)
  (and (number? x) (exact? x) (= x 0)))

(define (exact-one? x)
  (and (number? x) (exact? x) (= x 1)))

(define (log10 x)
  (/ (log x) :ln10))

(define (log2 x)
  (/ (log x) :ln2))

(define (exp10 x)
  (expt 10 x))

(define (exp2 x)
  (expt 2 x))

(define (safelog x)
  (if (and (real? x) (> x 0))
      (max (log x) :minlog)
      (error "Out of range -- SAFELOG" x)))

(define (principal-value cuthigh)
  (let ((cutlow (- cuthigh :+2pi)))
    (define (the-principal-value x)
      (if (and (<= cutlow x) (< x cuthigh))
	  x
	  (let ((y (- x (* :+2pi (floor (/ x :+2pi))))))
	    (if (< y cuthigh) 
		y
		(- y :+2pi)))))
    the-principal-value))


(define (principal-value-minus-pi-to-pi x)
  (if (or (<= x :-pi) (> x :+pi))
      (let ((y (- x (* :+2pi (floor (/ x :+2pi))))))
	(if (< y :+pi) 
	    y
	    (- y :+2pi)))
      x))

(define (principal-value-zero-to-2pi x)
  (if (or (< x 0.0) (>= x :+2pi))
      (- x (* :+2pi (floor (/ x :+2pi))))
      x))


(define ((principal-range period) index)
  (let ((t (- index (* period (floor (/ index period))))))
    (if (< t (/ period 2.))
        t
        (- t period))))



(define (one? x) (= x 1))		; Exactness?

(define (square x) (* x x))
(define (cube x) (* x x x))

(define (negate x) (- x))
(define (invert x) (/ x))

(define (cot x)
  (/ 1 (tan x)))

(define (sec x)
  (/ 1 (cos x)))

(define (csc x)
  (/ 1 (sin x)))

(define (sinh x)
  (/ (- (exp x) (exp (- x))) 2))

(define (cosh x)
  (/ (+ (exp x) (exp (- x))) 2))

(define (tanh x)
  (/ (sinh x) (cosh x)))

(define (sech x)
  (/ 1 (cosh x)))

(define (csch x)
  (/ 1 (sinh x)))


(define (exact-quotient n d)
  (define-values (q r) (quotient/remainder n d))
  (assert (= 0 r))
  q)


(define (binomial-coefficient n m)
  (assert (and (exact-integer? n) (exact-integer? m) (<= 0 m n)))
  (let ((d (- n m)))
    (let ((t (max m d)) (s (min m d)))
      (define (lp count prod)
	(if (= count t)
	    (exact-quotient prod (factorial s))
	    (lp (- count 1) (* count prod))))
      (lp n 1))))


(define (stirling-first-kind n k)
  (assert (and (int:<= 1 k) (int:<= k n)))
  (define lp
    (linear-memoize
     (lambda (n k)
       (cond ((int:= n 0)
	      (if (int:= k 0) 1 0))
	     (else
	      (let ((np (int:- n 1)))
		(int:+ (lp np (int:- k 1))
		       (int:* np
			      (lp np k)))))))
     0))
  (lp n k))

#|
(stirling-first-kind 1 1)
;Value: 1

(stirling-first-kind 2 1)
;Value: 1

(stirling-first-kind 2 2)
;Value: 1

(stirling-first-kind 3 1)
;Value: 2

(stirling-first-kind 3 2)
;Value: 3

(stirling-first-kind 5 2)
;Value: 50

(stirling-first-kind 7 3)
;Value: 1624
|#

(define (stirling-second-kind n k)
  (assert (and (int:<= 1 k) (int:<= k n)))
  (define lp
    (linear-memoize
     (lambda (n k)
       (cond ((int:= k 1) 1)
	     ((int:= n k) 1)
	     (else
	      (let ((np (int:- n 1)))
		(int:+ (* k (lp np k))
		       (lp np (int:- k 1)))))))
     0))
  (lp n k))

#|
(stirling-second-kind 5 3)
;Value: 25
|#

;;; Zero testing is hard, because of floating-point roundoff.

(define (~0? x [slack 100])
  (if (exact? x)
      (= x 0)
      (<= (magnitude x)
          (* slack
             *machine-epsilon*))))


(define (close-enuf? h1 h2 tolerance)
  (<= (magnitude (- h1 h2))
      (* .5 (max tolerance *machine-epsilon*)
	 (+ (magnitude h1) (magnitude h2) 2.0))))

#|
;;; When really paranoid, put in a scale factor

(define (close-enuf? h1 h2 #!optional tolerance scale)
  (if (default-object? tolerance)
      (set! tolerance *machine-epsilon*))
  (if (default-object? scale)
      (set! scale 1.0))
  (<= (magnitude (- h1 h2))
      (* tolerance
         (+ (* 0.5
	       (+ (magnitude h1) (magnitude h2)))
	    scale))))
|#


#|
;;; See below for the reason for using 
;;;  the more complex addition formula.

(define (sigma f low high)
  (let lp ((i low) (sum 0))
    (if (fix:> i high)
	sum
	(lp (fix:+ i 1) (+ sum (f i))))))
|#

(define (sigma f low high)
  (let lp ((i low) (sum 0) (c 0))
    (if (fix:> i high)
	sum
	(let* ((y (- (f i) c)) (t (+ sum y)))
	  (lp (fix:+ i 1) t (- (- t sum) y))))))

#|
;;; When adding up 1/n large-to-small we
;;; get a different answer than when adding 
;;; them up small-to-large, which is more
;;; accurate.

(let lp ((i 1) (sum 0.0))
  (if (> i 10000000)
      sum
      (lp (+ i 1) (+ sum (/ 1.0 i)))))
;Value: 16.695311365857272

(let lp ((i 10000000) (sum 0.0))
  (if (= i 0)
      sum
      (lp (- i 1) (+ sum (/ 1.0 i)))))
;Value: 16.695311365859965

(- 16.695311365859965 16.695311365857272)
;Value: 2.6929569685307797e-12

;;; Traditional sigma is the inaccurate way.
(sigma (lambda (x) (/ 1.0 x)) 1 10000000)
;Value: 16.695311365857272

;;; Kahan's compensated summation formula is
;;; much better, but slower...
(define (sigma-kahan f low high)
  (let lp ((i low) (sum 0) (c 0))
    (if (fix:> i high)
	sum
	(let* ((y (- (f i) c)) (t (+ sum y)))
	  (lp (fix:+ i 1) t (- (- t sum) y))))))
;Value: sigma-kahan

(sigma-kahan (lambda (x) (/ 1.0 x)) 1 10000000)
;Value: 16.69531136585985

(- 16.695311365859965 16.69531136585985)
;Value: 1.1368683772161603e-13
|#


#|
;;; Harmonic numbers

(define (Hn n)
  (/ (stirling-first-kind (+ n 1) 2)
     (factorial n)))

(exact->inexact (Hn 300))
;Value: 6.282663880299504

(let lp ((i 1) (sum 0.0))
  (if (> i 300)
      sum
      (lp (+ i 1) (+ sum (/ 1.0 i)))))
;Value: 6.282663880299502

(let lp ((i 300) (sum 0.0))
  (if (= i 0)
      sum
      (lp (- i 1) (+ sum (/ 1.0 i)))))
;Value: 6.282663880299501

(sigma (lambda (x) (/ 1.0 x)) 1 300)
;Value: 6.282663880299502
|#

#|
(define (geometric a r n)
  (define (sigma-kahan f low high)
    (let lp ((i low) (sum 0) (c 0))
      (if (fix:> i high)
	  sum
	  (let* ((y (- (f i) c)) (t (+ sum y)))
	    (lp (fix:+ i 1) t (- (- t sum) y))))))
  (let lp> ((k 0) (sum 0.0))
    (if (> k n)
	(write-line `(forward-order ,sum))
	(lp> (+ k 1) (+ sum (* a (expt r k))))))
  (write-line
   `(g:sigma
     ,(g:sigma				;generic sigma is naive
       (lambda (k)
	 (exact->inexact (* a (expt r k))))
       0 n)))
  (let lp< ((k n) (sum 0.0))
    (if (< k 0)
	(write-line `(reverse-order ,sum))
	(lp< (- k 1) (+ sum (* a (expt r k))))))
  (write-line
   `(kahan-method
     ,(sigma-kahan
       (lambda (k)
	 (exact->inexact (* a (expt r k))))
       0 n)))  
  (write-line
   `(explicit-formula
     ,(exact->inexact (/ (* a (- 1 (expt r (+ n 1))))
			 (- 1 r)))))
  'done)

(geometric 1 0.5001 200)
(forward-order 2.0004000800160022)
(g:sigma 2.0004000800160022)
(reverse-order 2.000400080016003)
(kahan-method 2.000400080016003)
(explicit-formula 2.000400080016003)
;Value: done
|#


;;; The following is arbitrary, but chosen to make Euclid's algorithm 
;;; for polynomials over the rationals (defined with pseudo-division)
;;; have only small fractions.


#| Wrong    
(define make-rational
  (access make-rational (->environment '(runtime number))))

(define (gcd-rational p/q r/s)
  (make-rational (gcd (numerator p/q) (numerator r/s))
		 (lcm (denominator p/q) (denominator r/s))))
|#

(define (gcd-rational p/q r/s)
  (gcd (numerator p/q) (numerator r/s)))


;;; Euclid's algorithm for Exact Complex Numbers
;;;   suggested by William Throwe, to allow simplification
;;;   of rational functions with complex coefficients.

(define (round-complex z)
  (make-rectangular (round (real-part z))
		    (round (imag-part z))))

(define (gcd-complex a b)
  (cond ((zero? a) b)
        ((zero? b) a)
        (else
         (let ((q (round-complex (/ a b))))
           (gcd-complex b (- a (* q b)))))))

(define (exact-complex? x)
  (and (number? x) (exact? x)))

(define (scheme-number-gcd x y)
  (cond ((or (inexact? x) (inexact? y)) 1)
	((and (exact-integer? x) (exact-integer? y))
	 (gcd x y))
	((and (exact-rational? x) (exact-rational? y))
	 (gcd-rational x y))
	((and (exact-complex? x) (exact-complex? y))
	 (gcd-complex x y))
	(else 1)))


(define (scheme-number-divide n d c)
  (if (and *no-rationals-in-divide*
	   (exact-integer? n)
	   (exact-integer? d))
      (let-values ([(q r) (quotient/remainder n d)])
	(c q r))
      (c (/ n d) 0)))



(define (sgn x)
  (if (real? x)
      (if (negative? x) -1 +1)
      ;; This is a kludge, needed so that rational 
      ;; functions can be canonicalized, even if 
      ;; coefficients are complex.
      (if (negative? (real-part x)) -1 +1)))
  


;;; From Hamming, gives roots of quadratic without bad roundoff.
;;; a*x^2 + b*x + c = 0

(define (quadratic a b c
		   ;; continuations for each case
		   two-roots
		   [complex-roots default-object]
		   [double-root default-object]
		   [linear default-object]
		   [no-solution default-object])
  (if (zero? a)
      (if (zero? b)
	  (if (default-object? no-solution)
	      (error "No solution -- QUADRATIC" a b c)
	      (no-solution a b c))
	  (if (default-object? linear)
	      (error "Not QUADRATIC" a b c)
	      (linear (/ (- c) b))))
      (let ((d (- (* b b) (* 4 a c))))
	(if (zero? d)
	    (let ((root (/ b (* -2 a))))
	      (if (default-object? double-root)
		  (two-roots root root)
		  (double-root root)))
	    (let ((q (* -1/2 (+ b (* (sgn b) (sqrt d))))))
	      (let ((r1 (/ q a)) (r2 (/ c q)))
		(if (or (> d 0)
			(default-object? complex-roots))
		    (two-roots r1 r2)
		    (complex-roots r1 r2))))))))


;;; From Numerical Recipes... for real coefficients
;;; x^3 + a*x^2 + b*x + c = 0

(define (cubic a b c cont)
  (let ((q (/ (- (square a) (* 3 b)) 9))
	(r (/ (+ (* 2 (cube a)) (* -9 a b) (* 27 c)) 54)))
    (let ((aa
	   (* -1
	      (sgn r)
	      (expt (+ (abs r)
		       (sqrt (- (square r) (cube q))))
		    1/3))))
      (let ((bb (if (zero? aa) 0 (/ q aa))))
	(let ((u (+ aa bb))
	      (v (* -1/3 a))
	      (w (* (/ (sqrt 3) 2) (- aa bb))))
	  (cont (+ u v)
		(+ (* -1/2 u) v (* +i w))
		(+ (* -1/2 u) v (* -i w))))))))
