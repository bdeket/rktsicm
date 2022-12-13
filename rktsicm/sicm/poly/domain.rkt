#lang racket/base

(provide (all-defined-out))

(require (only-in "../rkt/glue.rkt" if)
         "../kernel-gnrc.rkt"
         "../simplify/pcf.rkt"
         )

;;bdk;; From polyinterp.scm

;;bdk;; insert 1
;;; Alter the coefficients of polynomial P so that its domain [a,b] is
;;;  mapped onto the canonical domain [-1,1].

(define (poly-domain->canonical p a b)
  (if (<= b a)
      (error "bad interval: must have a < b in POLY-DOMAIN->CANONICAL"))
  (let ((c (/ (+ a b) 2)) (d (/ (- b a) 2)))
    ;; p(x) [a,b] --> p(y+c) = q(y) [-d,d] --> q(d*z) = r(z) [-1,1]
    (poly:arg-scale (poly:arg-shift p (list c)) (list d))))


;;; Alter the coefficients of polynomial P so that its domain [-1,1]
;;;  is mapped onto [a,b].  This is the inverse operation to
;;;  POLY-DOMAIN->CANONICAL.

(define (poly-domain->general p a b)
  (if (<= b a)
      (error "bad interval: must have a < b in POLY-DOMAIN->GENERAL"))
  (let ((c (/ (+ a b) 2)) (d (/ (- b a) 2)))
    (poly:arg-shift (poly:arg-scale p (list (/ 1 d))) (list (- c)))))
;;bdk;; insert 1 end
