#lang racket/base

(provide (all-defined-out))

(require "../kernel-gnrc.rkt"
         "Hamiltonian.rkt"
         "Lagrangian.rkt"
         )

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
