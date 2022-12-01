#lang s-exp "../generic.rkt"

(provide (all-defined-out))

(require "canonical.rkt"
         "Hamiltonian.rkt"
         "point-transformation.rkt"
         "universal.rkt"
         )

;;; This is used in conjunction with a symplectic test for the C to
;;; establish that a time-dependent transformation is canonical.

;;; To compute the K (addition to the Hamiltonian) from a
;;; time-dependent coordinate transformation F.

(define ((F->K F) s)
  (* -1 (((partial 0) F) s) (momentum ((F->CT F) s))))


;;; Tests that K yields a canonical transformation if the C is
;;; symplectic.  (The qp-canonical? code is really a symplectic 
;;; test without factoring out the Hamiltonian.)

(define ((qp-canonical? C H) s)
  (- (J-func ((D H) (C s)))
     (* ((D C) s)
	(J-func
	 ((D (compose H C)) s)))))

#|
(define ((canonical-K? C K) s)
  (let ((s* (compatible-shape s)))
    (- (T-func s*)
       (+ (* ((D C) s) (J-func ((D K) s)))
	  (((partial 0) C) s)))))


(define ((canonical-K? C K) s)
  (let ((DCs ((D C) s))
	(s* (compatible-shape s)))
    (- (T-func s*)
       (* DCs ((Hamiltonian->state-derivative K) s)))))
|#

#|
(define ((rotating n) state)
  (let ((t (time state))
	(q (coordinate state)))
    (let ((x (ref q 0))
	  (y (ref q 1))
	  (z (ref q 2)))
      (coordinate-tuple (+ (* (cos (* n t)) x) (* (sin (* n t)) y))
			(- (* (cos (* n t)) y) (* (sin (* n t)) x))
			z))))

(define (C-rotating n) (F->CT (rotating n)))

(define ((K n) s)
  (let ((q (coordinate s))
	(p (momentum s)))
    (let ((x (ref q 0)) (y (ref q 1))
	  (px (ref p 0)) (py (ref p 1)))
      (* n (- (* x py) (* y px))))))

(define a-state 
  (up 't 
      (coordinate-tuple 'x 'y 'z)
      (momentum-tuple 'p_x 'p_y 'p_z)))


(pe ((canonical-K? (C-rotating 'n) (K 'n)) a-state))
(up 0 (up 0 0 0) (down 0 0 0))

;;; or getting K directly from F
(pe ((canonical-K? (C-rotating 'n) (F->K (rotating 'n))) a-state))
(up 0 (up 0 0 0) (down 0 0 0))

(pe ((- (F->K (rotating 'n))
	(K 'n))
     a-state))
0

;;; not all K's work

(define ((bad-K n) s)
  (- ((K n) s)))

(pe ((canonical-K? (C-rotating 'n) (bad-K 'n)) a-state))
(up
 0
 (up (+ (* 2 n x (sin (* n t))) (* -2 n y (cos (* n t))))
     (+ (* 2 n x (cos (* n t))) (* 2 n y (sin (* n t))))
     0)
 (down (+ (* 2 n p_x (sin (* n t))) (* -2 n p_y (cos (* n t))))
       (+ (* 2 n p_x (cos (* n t))) (* 2 n p_y (sin (* n t))))
       0))
|#
