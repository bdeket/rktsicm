#lang s-exp "../generic.rkt"

(provide (all-defined-out))

(require "../general/list-utils.rkt"
         "universal.rkt"
         )

;;; Noether Theorem Support

;;; F-tilde is a parametric coordinate transformation that given
;;; parameters takes a state and returns transformed coordinates.   
;;; F-tilde may take an arbitrary number of real-valued parameters.
;;; F-tilde applied to zeros is the coordinate selector:  It takes a
;;; state and returns the coordinates.  The hypothesis of Noether's
;;; theorem is that the Lagrangian is invariant under the
;;; transformation for all values of the parameters.

;;; (D (lambda parms (compose L (F->C (apply F-tilde parms))))) = 0

(define (Noether-integral L F-tilde)
  (let ((zero-parameters (make-list (arity-min (arity F-tilde)) 0)))
    (* ((partial 2) L) (apply (D F-tilde) zero-parameters))))

#|
(define ((L-central-rectangular m V) local)
  (let ((q (coordinate local))
        (v (velocity local)))
    (- (* 1/2 m (square v))
       (V (sqrt (square q))))))

(define (F-tilde theta phi psi)
  (compose (Rx theta)
	   (Ry phi)
	   (Rz psi)
	   coordinate))

(pe ((Noether-integral
      (L-central-rectangular 'm (literal-function 'Vr))
      F-tilde)
     (up 't
	 (up 'x 'y 'z)
	 (up 'vx 'vy 'vz))))
(down (+ (* -1 m vy z) (* m vz y))
      (+ (* m vx z) (* -1 m vz x))
      (+ (* -1 m vx y) (* m vy x)))
|#

