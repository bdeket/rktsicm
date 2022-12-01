#lang s-exp "../generic.rkt"

(provide (all-defined-out))

(require "Lagrangian.rkt"
         "Hamiltonian.rkt"
         )
;;; Makes a canonical point transformation from a 
;;;  time-invariant coordinate transformation T(q)

(define ((F->CH F) H-state)
  (up (time H-state)
      (F H-state)
      (solve-linear-right (momentum H-state)
                          (((partial 1) F) H-state))))

(define F->CT F->CH)

(define ((F->K F) H-state)
  (- (* (solve-linear-right (momentum H-state)
                            (((partial 1) F) H-state))
        (((partial 0) F) H-state))))

#|
;;; For display in book -- assumes flat coordinates

(define ((F->CH F) H-state)
  (->H-state (time H-state)
             (F H-state)
             (* (momentum H-state)
                (invert (((partial 1) F) H-state)))))

(define ((F->K F) H-state)
  (- (* (* (momentum H-state)
	   (invert (((partial 1) F) H-state)))
	(((partial 0) F) H-state))))

|#
#|

(define ((H-central m V) state)
  (let ((x (coordinate state))
	(p (momentum state)))
    (+ (/ (square p) (* 2 m))
       (V (sqrt (square x))))))

(show-expression
 ((compose (H-central 'm (literal-function 'V))
           (F->CT p->r))
  (->H-state 't
             (coordinate-tuple 'r 'phi)
             (momentum-tuple 'p_r 'p_phi))))
(+ (V r)
   (/ (* 1/2 (expt p_r 2)) m)
   (/ (* 1/2 (expt p_phi 2)) (* m (expt r 2))))

|#

