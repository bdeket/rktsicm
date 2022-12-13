#lang s-exp "../generic.rkt"

(provide (all-defined-out))

(require (only-in "../rkt/glue.rkt" iota)
         "../general/permute.rkt"
         )

;;bdk;; start original file

;;; Gram-Schmidt orthonormalization process...

(define (Gram-Schmidt vector-basis metric)
  (define (make-positive x)
    (if (and (number? x) (real? x))
        (if (negative? x)
            (- x)
            x)
	(sqrt (square x))))
  (define (normalize v)
    (* (/ 1 (sqrt (make-positive (metric v v)))) v))
  (let lp ((ins (ultra-flatten vector-basis)) (outs '()))
    (if (null? ins)
        (apply down (reverse outs))
        (let ((q (normalize (car ins))))
          (lp (map (lambda (v)
                     (- v (* (metric q v) q)))
                   (cdr ins))
              (cons q outs))))))

#|
;;; Orthonormalizing with respect to the Lorentz metric in 2 dimensions. 

(install-coordinates R2-rect (up 't 'x))
(define R2-point ((R2-rect '->point) (up 't0 'x0)))
(define R2-basis (coordinate-system->basis R2-rect))

(define ((L2-metric c) u v)
  (+ (* -1 c c (dt u) (dt v))
     (* 1 (dx u) (dx v))))

(define L2-vector-basis
  (Gram-Schmidt (basis->vector-basis R2-basis) (L2-metric 'c)))

(s:foreach (lambda (v)
	     (pec ((v (literal-manifold-function 'f R2-rect))
		  R2-point)))
	   L2-vector-basis)
#| Result:
(/ (((partial 0) f) (up t0 x0)) c)
|#
#| Result:
(((partial 1) f) (up t0 x0))
|#
;Value: done

(s:foreach (lambda (omega)
	     (pec ((omega (literal-vector-field 'v R2-rect))
		  R2-point)))
	   (vector-basis->dual L2-vector-basis R2-rect))
#| Result:
(* c (v^0 (up t0 x0)))
|#
#| Result:
(v^1 (up t0 x0))
|#
|#

#|
;;; 4-dimensional Lorentz metric.

(define SR R4-rect)
(install-coordinates SR (up 't 'x 'y 'z))

(define ((g-Lorentz c) u v)
  (+ (* (dx u) (dx v))
     (* (dy u) (dy v))
     (* (dz u) (dz v))
     (* -1 (square c) (dt u) (dt v))))

(define SR-basis (coordinate-system->basis SR))

(define an-event ((SR '->point) (up 't0 'x0 'y0 'z0)))

(define SR-V (basis->vector-basis SR-basis))

(define SR-V1 (ultra-flatten (Gram-Schmidt SR-V (g-Lorentz 'c))))

;;; SR-V1 is orthogonal
(for-each (lambda (v1)
	    (for-each (lambda (v2)
			(pe (((g-Lorentz 'c) v1 v2) an-event)))
		      (cdr (memq v1 SR-V1))))
	  SR-V1)
0
0
0
0
0
0

;;; SR-V1 is normal
(for-each (lambda (v)
	    (pe (((g-Lorentz 'c) v v) an-event)))
	  SR-V1)
-1
1
1
1

(for-each (lambda (v)
	    (pe ((v (SR '->coords))
		 an-event)))
	  SR-V1)
(up (/ 1 c) 0 0 0)
(up 0 1 0 0)
(up 0 0 1 0)
(up 0 0 0 1)
|#

#|
(install-coordinates R3-rect (up 'x 'y 'z))
(define R3-point ((R3-rect '->point) (up 'x0 'y0 'z0)))
(define R3-basis (coordinate-system->basis R3-rect))

(define ((g3-maker a b c d e f) v1 v2)
  (+ (* a (dx v1) (dx v2))
     (* b (dx v1) (dy v2))
     (* c (dx v1) (dz v2))
     (* b (dx v2) (dy v1))
     (* d (dy v1) (dy v2))
     (* e (dy v1) (dz v2))
     (* c (dx v2) (dz v1))
     (* e (dy v2) (dz v1))
     (* f (dz v1) (dz v2))))

(define g3 (g3-maker 'a 'b 'c 'd 'e 'f))

(for-each (lambda (v)
	    (pe ((v (R3-rect '->coords))
		 R3-point)))
	  (ultra-flatten
	   (Gram-Schmidt
	    (basis->vector-basis R3-basis)
	    g3)))
(up (/ 1 (sqrt a)) 0 0)
(up (/ (* -1 b) (sqrt (+ (* (expt a 2) d) (* -1 a (expt b 2)))))
    (/ a (sqrt (+ (* (expt a 2) d) (* -1 a (expt b 2)))))
    0)
;;; Third one is something awful...
|#

;;; This may be needed... ugh!

(define (completely-antisymmetric indices)
  (permutation-parity indices (iota (length indices))))


