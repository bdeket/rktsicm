#lang s-exp "../generic.rkt"

(provide (all-defined-out))

(require "Hamiltonian.rkt"
         "Lagrangian.rkt"
         )

;;bdk;; start original file

;;; time evolution transformations

(define ((shift-t delta-t) state)
  (->H-state 
   (+ (time state) delta-t)
   (coordinate state)
   (momentum state)))

(define ((C->Cp C) delta-t)
  (compose (C delta-t) (shift-t (- delta-t))))

(define ((H->Hp delta-t) H)
  (compose H (shift-t (- delta-t))))
