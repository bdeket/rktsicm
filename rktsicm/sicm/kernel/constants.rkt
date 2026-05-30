#lang racket/base

(provide (except-out (all-defined-out) :pi-setter))
(module+ setter (provide :pi-setter))

(define n:zero 0)
(define n:one 1)
(define n:-one -1)

(define n:pi (atan 0 -1))
(define n:+pi n:pi)
(define n:-pi (- n:pi))
(define n:pi/6 (/ n:pi 6))
(define n:+pi/6 n:pi/6)
(define n:-pi/6 (- n:pi/6))
(define n:pi/4 (atan 1 1))
(define n:+pi/4 n:pi/4)
(define n:-pi/4 (- n:pi/4))
(define n:pi/3 (/ n:pi 3))
(define n:+pi/3 n:pi/3)
(define n:-pi/3 (- n:pi/3))
(define n:pi/2 (atan 1 0))
(define n:+pi/2 n:pi/2)
(define n:-pi/2 (- n:pi/2))
(define n:2pi (* 2 n:pi))
(define n:+2pi n:2pi)
(define n:-2pi (- n:2pi))

;;; n:machine-epsilon is the smallest number that when added to 1.0
;;;  gives a different number.

(define n:machine-epsilon
  (let loop ((e 1.0))
    (if (= 1.0 (+ e 1.0))
        (* 2 e)
        (loop (/ e 2)))))

;;; In 64-bit IEEE-754 floating point
;;; n:machine-epsilon = 2.220446049250313e-16 = 2^(-52)


(define n:sqrt-machine-epsilon
  (sqrt n:machine-epsilon))

(define n:euler 0.57721566490153286)

(define n:phi (/ (+ 1 (sqrt 5)) 2))

(define n:ln2 (log 2.0))
(define n:ln10 (log 10.0))

(define symb:pi ':pi)
(define :pi n:pi)
(define (:pi-setter x) (set! :pi x))
