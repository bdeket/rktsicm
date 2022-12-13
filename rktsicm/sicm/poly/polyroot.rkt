#lang racket/base

(provide (all-defined-out))

(require (only-in "../rkt/glue.rkt" if random for-all? write-line false
                  fix:= fix:< fix:+)
         (only-in "../rkt/define.rkt" define default-object?)
         "../general/list-utils.rkt"
         "../kernel-intr.rkt"
         "../simplify/pcf.rkt"
         "../numerics/statistics/cluster.rkt"
         )

;;bdk;; start original file

;;; General Polynomial Root Finder


(define (complex-random modulus)
  (make-polar modulus (random :2pi)))


;;; Gets the value of p at x, and the error estimate, by Horners rule
;;; defined in pcf.scm.

(define (horners-rule p x)
  (poly:horners-rule-with-error p x list))


(define (roots->poly roots)
  ;;bdk;; reduce needs tree arguments... maybe a-reduce?
  (a-reduce poly:*
	    (map (lambda (r) (poly:- poly:identity r))
		 roots)))

;;; This finds the roots of a univariate polynomial.

(define (poly->roots given-poly #:optional expand-multiplicities?)
  (let* ((given-poly (ensure-real given-poly))
	 (all-real?
	  (for-all? (poly/coefficients given-poly)
                    (lambda (c) (zero? (imag-part c))))))
    (define (search kernel-poly initial-roots)
      (let ((polish (root-polisher kernel-poly)))
	(let find-loop ((deflated-poly kernel-poly) (roots initial-roots))
	  (if root-wallp
              (write-line (list 'finder-loop deflated-poly roots)))
	  (if (fix:< (poly:degree deflated-poly) 1)
	      (if (not (fix:= (length roots) (poly:degree kernel-poly)))
		  (begin (error "Root finder failed" kernel-poly) 'foo)
                  roots)
	      (let ((root
		     (clean-up-root
		      (polish
		       (rescale-poly-roots deflated-poly root-searcher)))))
		(if (and all-real? (obviously-complex? root))
		    (let ((cr (conjugate root)))
		      (find-loop (ensure-real
				  (deflate-poly deflated-poly
                                    (list root cr)))
				 (cons root (cons cr roots))))
		    (find-loop (deflate-poly deflated-poly (list root))
			       (cons root roots))))))))
    (let ((n (poly:degree given-poly))
	  (m (poly:lowest-order given-poly)))
      (cond ((fix:< n 1) '())
	    ((fix:= m 0)
	     (cluster-multiple-roots (search given-poly '())
                                     expand-multiplicities?))
	    (else	;factors of the indeterminate to be removed.
	     (let ((zero-roots (make-list m 0)))
	       (cluster-multiple-roots
                (append zero-roots
                        (search (deflate-poly given-poly zero-roots)
                                '()))
                expand-multiplicities?)))))))

(define (cluster-multiple-roots roots expand-multiplicities?)
    (if (default-object? expand-multiplicities?)
      (set! expand-multiplicities? #t))
    (let ((rs
         (sort (identify-multiple-roots roots)
               (lambda (r1 r2)
                 (let ((mr1 (magnitude (cdr r1)))
                       (mr2 (magnitude (cdr r2))))
                   (or (< mr1 mr2)
                       (and (= mr1 mr2)
                            (< (real-part (cdr r1))
                               (real-part (cdr r2))))))))))
    (if expand-multiplicities?
        (expand-multiplicities rs)
        rs)))

(define (ensure-real poly)
  (let lp ((poly poly))
    (if (pair? poly)
	(cons (lp (car poly)) (lp (cdr poly)))
	(let ((c poly))
	  (if (and (number? c) (inexact? c))
	      (bring-to-real c)
	      c)))))

(define (bring-to-real c)
  (if (< (abs (imag-part c))
	 (* imaginary-part-tolerance
            *machine-epsilon*
            (abs (real-part c))))
      (real-part c)
      c))

;;; Heuristic test

(define (obviously-complex? root)
  (and (> (magnitude root) minimum-magnitude)
       (or (and (zero? (real-part root)) (not (zero? (imag-part root))))
	   (> (abs (imag-part root))
	      (* (abs (real-part root))
		 obviousity-factor)))))

;;; The following gets rid of microscopic imaginary parts or real
;;; parts that may arise from roundoff errors.

(define (clean-up-root root)
  (let ((rt (* rationalization-tolerance *machine-epsilon*)))
    (cond ((zero? root) root)
	  ((real? root) (rationalize root rt))
	  (else
	   (let ((rr (rationalize (real-part root) rt))
		 (ri (rationalize (imag-part root) rt)))
	     (let ((ar (abs rr)) (ai (abs ri)))
	       (cond ((> (* ar on-axis-tolerance *machine-epsilon*) ai)
		      rr)
		     ((> (* ai on-axis-tolerance *machine-epsilon*) ar)
		      (* ri +i))
		     (else
		      (make-rectangular rr ri)))))))))

(define (rescale-poly-roots poly searcher)
  (let ((Nn (poly:degree poly))
	(N0 (poly:lowest-order poly)))
    (if (fix:= Nn N0)
	0
	(let ((An (poly:leading-coefficient poly))
	      (A0 (poly:trailing-coefficient poly)))
	  (let ((k (/ A0 An)))
	    (let ((c (expt k (/ 1.0 (- Nn N0)))))
	      (if (< (/ 1.0 max-scale) (magnitude k) max-scale)
		  (searcher poly)
		  (* (searcher (poly:* (poly:arg-scale poly (list c))
				       (/ 1.0 (* A0 (expt c N0)))))
		     c))))))))

;;; Deflation

(define (deflate-poly p roots)
  (poly:divide p 
	       (roots->poly roots)
	       (lambda (q r)
		 ;; We will test r for approximate zero to be sure
		 (if root-wallp (write-line (list q r)))
		 q)))

;;; Clustering roots of polynomials to determine multiplicities can
;;; be used to improve the accuracy, by a theorem of Wilkenson.

(define identify-multiple-roots
  (let ()
    (define (mult-scan cl)
      (let ((n (length (cluster-elements cl))))
	(if (fix:= n 1)
	    (list (cons 1 (car (cluster-elements cl))))
	    (let ((mid (/ (apply + (cluster-elements cl)) n)))
	      (if (< (cluster-diameter cl)
		     (expt (* cluster-tolerance
			       (+ 1.0 (magnitude mid))
			       *machine-epsilon*)
			   (/ 1.0 n)))
		  (list (cons n (clean-up-root mid)))
		  (append (mult-scan (car (cluster-subclusters cl)))
			  (mult-scan (cadr (cluster-subclusters cl)))))))))
    (define (d z1 z2) (magnitude (- z1 z2)))
    (define sd (set-separation d))
    (lambda (roots)
      (cond ((null? roots) '())
	    (clustering (mult-scan (car (cluster roots sd d))))
	    (else (map (lambda (r) (cons 1 r)) roots))))))

;;; To undo this damage:

(define (expand-multiplicities roots)
  (if (null? roots)
      '()
      (append (make-list (caar roots) (cdar roots))
	      (expand-multiplicities (cdr roots)))))

;;; Control for root-finder search

(define (root-searcher p)
  (let ((improve (root-searcher-method p)))
    (define (try xn xn-1 vxn-1 dvxn-1 iter-count shrink-count)
      (let* ((xn (bring-to-real xn))
	     (vxn/err (horners-rule p xn))
	     (dx (- xn xn-1)))
	(let ((vxn (car vxn/err))
              (dvxn (cadr vxn/err))
              (err (cadddr vxn/err))
              (iter-count (fix:+ iter-count 1)))
	  (if root-wallp (write-line `(trying ,xn ,vxn ,err)))
	  (cond ((> iter-count root-searcher-max-iter)
                 #f)                    ;failed
                ((< (magnitude vxn)
		    (* root-searcher-value-to-noise (magnitude err)))
		 (if root-wallp
                     (write-line `(found-winner-at ,xn ,vxn ,err)))
		 xn)			;good enuf.
		((< (magnitude dx)
		    (* root-searcher-minimum-progress *machine-epsilon*
		       (magnitude xn)))
		 (if root-wallp
		     (write-line `(found-lazy-winner-at ,xn ,vxn ,err)))
		 xn)
		((and (not (< (magnitude dvxn-1) minimum-denominator))
		      (not (< (magnitude dvxn) minimum-denominator))
                      (kahan-secant-method xn xn-1 vxn vxn-1 dvxn dvxn-1
                                           try iter-count)))
		((and (<= (magnitude vxn) (magnitude vxn-1))
                      (improve xn vxn/err
			       (lambda (xn+1)
			         (try xn+1 xn vxn dvxn iter-count 0))
			       (lambda ()
			         (if root-wallp
				     (write-line
                                      `(zero-divide-at ,xn in searcher)))
			         xn))))
                (else
                 (wrong-way xn xn-1 vxn-1 dvxn-1
                            try iter-count shrink-count))))))
    (or (start-search p root-searcher-x0 try improve)
        (error "Search failed" p))))

(define (start-search p initial-x0 try improve)
  (let ((vx0/err (horners-rule p initial-x0)))
      (if root-wallp
          (write-line `(hunting-starting-at ,initial-x0 ,p)))
      (let ((vx0 (car vx0/err))
            (dvx0 (cadr vx0/err))
            (err0 (cadddr vx0/err)))
	(if (< (magnitude vx0)
	       (* root-searcher-value-to-noise (magnitude err0)))
	    (begin
	      (if root-wallp
		  (write-line
		   `(won-immediately-at ,initial-x0 ,vx0 ,err0)))
	      initial-x0)	;good enuf.
	    (improve initial-x0 vx0/err
		     (lambda (x1)
                       (try x1 initial-x0 vx0 dvx0 1 0))
		     (lambda ()
		       (if root-wallp
			   (write-line
			    `(zero-divide-at ,initial-x0
					     in startup of searcher)))
		       initial-x0))))))

(define (kahan-secant-method xn xn-1 vxn vxn-1 dvxn dvxn-1 try iter-count)
  (let* ((f (/ vxn dvxn))
         (d (- f (/ vxn-1 dvxn-1))))
    (and (not (< (magnitude d) minimum-denominator))
         (let* ((q (magnitude (/ (- xn xn-1) d)))
                (iq (round q)))
           (and (> iq 1)
                (< (abs (- q iq)) (* *kahan-threshold* q))
                (begin (if root-wallp
                           (write-line
                            `(trying-kahan-trick ,iq)))
                       (try (- xn (* f iq))
                            xn vxn dvxn iter-count 0)))))))

(define (wrong-way xn xn-1 vxn-1 dvxn-1 try iter-count shrink-count)
  (cond ((< shrink-count root-searcher-max-shrink)
         ;; Cuthbert's hack -- fatal with Laguerre method!
         ;;  try x^40 + 1 = 0
	 (try (+ xn-1 (/ (- xn xn-1) root-searcher-shrink-factor))
	      xn-1 vxn-1 dvxn-1 
	      iter-count (fix:+ shrink-count 1)))
	(else
         ;; Try a desparate root-searcher-jiggle
	 (try (+ xn-1
		 (complex-random
		  (* iter-count (magnitude xn-1)
		     root-searcher-jiggle)))
	      xn-1 vxn-1 dvxn-1 iter-count 0))))

(define (root-polisher p)
  (let ((improve (root-polisher-method p)))
    (define (try x vp-err)
      (let ((vp (car vp-err)) (vperr (cadddr vp-err)))
	(if root-wallp (write-line `(polishing root ,x ,vp ,vperr)))
	(if (< (magnitude vp)		;x is good enough
	       (* root-polisher-value-to-noise (magnitude vperr)))
	    (begin (if root-wallp (write-line `(win-at ,x)))
		   x)
	    (improve x vp-err
		     (lambda (nx)
		       (if (< (magnitude (- x nx)) ;insufficient improvement
			      (* root-polisher-minimum-progress
				 *machine-epsilon*
				 (+ (magnitude x) 1.0)))
			   (begin
			     (if root-wallp
				 (write-line `(good-enuf-at ,nx)))
			     nx)
			   (let* ((nvp-err (horners-rule p nx))
				  (nvp (car nvp-err)))
			     (if (< (magnitude nvp) (magnitude vp))
				 (try nx nvp-err)
				 (begin
				   (if root-wallp
				       (write-line `(got-worse-at ,nx)))
				   x)))))
		     (lambda ()
		       ; (error "zero-divide failure")
		       (if root-wallp
			   (write-line `(zero-divide-at ,x in polisher)))
		       x)))))
    (lambda (x) (try x (horners-rule p x)))))

;;; Newton's method
;;;   To find a root of P(x)
;;;     Start with a guess of X0 and iterate the following steps
;;;     Xn+1 = Xn - P(Xn)/P'(Xn)

(define (poly-newton-method p)
  (define (newton-improve x vp/err succeed fail)
    (let ((vp (car vp/err)) (dvp (cadr vp/err)))
      (if (< (magnitude dvp) minimum-denominator)
	  (fail)
	  (succeed (- x (/ vp dvp))))))
  newton-improve)

;;; Laguerre's method
;;;   To find a root of P(x)
;;;     Start with a guess of X0 and iterate the following steps
;;;       Let G = P'(Xn)/P(Xn)
;;;       Let H = G^2 - P''(Xn)/P(Xn)
;;;       Xn+1 = Xn - n/(G +- sqrt((n-1)*(n*H-G^2)))

(define (poly-laguerre-method p)
  (let ((n (poly:degree p)))
    (define (laguerre-improve x vp/err succeed fail)
      (let ((vp (car vp/err)) (vdp (cadr vp/err)) (vddp (caddr vp/err)))
	(if (< (magnitude vp) minimum-denominator)
	    (fail)
	    (let* ((g (/ vdp vp))
		   (g2 (* g g))
		   (d (sqrt (* (- n 1) (- (* n (- g2 (/ vddp vp))) g2))))
		   (gplus (+ g d))
		   (gminus (- g d))
		   (denom (if (> (magnitude gplus) (magnitude gminus))
			      gplus
			      gminus)))
	      (succeed (- x
			  (if (< (magnitude denom)
                                 (* n minimum-denominator))
			      (expt (magnitude
                                     (/ vp
                                        (poly:leading-coefficient p)))
				    (/ 1.0 n))
			      (/ n denom))))))))
    laguerre-improve))

(define root-wallp false)

(define minimum-magnitude 1e-10)
(define obviousity-factor 1e-3)

(define imaginary-part-tolerance 10.0)	;in machine-epsilons

(define on-axis-tolerance 1000.0)	;in machine-epsilons
(define rationalization-tolerance 100.0)

(define max-scale 1.0e30)

(define clustering #t)			;Turns on the clustering process
(define cluster-tolerance 1.0e2)

(define minimum-denominator 1e-100)

(define *kahan-threshold* .01)

(define root-searcher-method poly-laguerre-method)
(define root-searcher-x0 .1+.1i)	;.1+1i	;cuthbert
(define root-searcher-max-iter 100)
(define root-searcher-max-shrink 10)
(define root-searcher-jiggle .1)
(define root-searcher-shrink-factor 4)
(define root-searcher-value-to-noise 0.75)
(define root-searcher-minimum-progress 1.0)

(define root-polisher-method poly-newton-method)
(define root-polisher-value-to-noise 0.75)
(define root-polisher-minimum-progress 1.0)

