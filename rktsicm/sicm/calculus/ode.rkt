#lang s-exp "../generic.rkt"

(provide (all-defined-out))

(require (only-in "../mechanics/universal.rkt" D)
         )

;;bdk;; start original file

#|
;;; Let (sigma t) be the state of a system at time t.  Let the
;;; (first-order) system of differential equations governing the
;;; evolution of this state be:

;;;  ((D sigma) t) = (R (sigma t))  
;;;     or  (D sigma) = (compose R sigma)

;;; i.e. R is a system derivative.

;;; Let F be any function of state, then a differential equation for
;;; the evolution of F, as it is dragged along the integral curve
;;; sigma is:

;;; (D (compose F sigma)) = (* (compose (D F) sigma) (D sigma))
;;;                       = (compose (* (D F) R) sigma)

;;; Let's call this operation Lie-D (the Lie derivative for
;;; coordinates).  We define
|#

(define (Lie-D R)
  (define (the-LD F) (* (D F) R))
  (make-operator the-LD `(Lie-D ,R)))

    