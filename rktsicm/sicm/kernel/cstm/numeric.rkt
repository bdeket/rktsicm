#lang racket/base

(provide (all-defined-out))
;;;; Extensions to Scheme numbers

;;; Everybody wants to know about these.

(define zero 0)
(define one 1)
(define -one -1)
(define two 2)
(define three 3)

(define pi (* 4 (atan 1 1)))
(define -pi (- pi))
(define pi/6 (/ pi 6))
(define -pi/6 (- pi/6))
(define pi/4 (/ pi 4))
(define -pi/4 (- pi/4))
(define pi/3 (/ pi 3))
(define -pi/3 (- pi/3))
(define pi/2 (/ pi 2))
(define -pi/2 (- pi/2))
(define 2pi (+ pi pi))
(define -2pi (- 2pi))

(define :zero zero)
(define :one one)
(define :-one -one)
(define :two two)
(define :three three)

(define :pi pi)
(define :+pi pi)
(define :-pi -pi)
(define :pi/6 pi/6)
(define :+pi/6 pi/6)
(define :-pi/6 -pi/6)
(define :pi/4 pi/4)
(define :+pi/4 pi/4)
(define :-pi/4 -pi/4)
(define :pi/3 pi/3)
(define :+pi/3 pi/3)
(define :-pi/3 -pi/3)
(define :pi/2 pi/2)
(define :+pi/2 pi/2)
(define :-pi/2 -pi/2)
(define :2pi 2pi)
(define :+2pi 2pi)
(define :-2pi -2pi)


;;; *machine-epsilon* is the smallest number that when added to 1.0
;;;  gives a different number.

(define *machine-epsilon*
  (let loop ((e 1.0))
     (if (= 1.0 (+ e 1.0))
         (* 2 e)
         (loop (/ e 2)))))

;;; In 64-bit IEEE-754 floating point 
;;; *machine-epsilon* = 2.220446049250313e-16 = 2^(-52)

(define *sqrt-machine-epsilon* 
  (sqrt *machine-epsilon*))

(define :euler 0.57721566490153286)

(define :phi (/ (+ 1 (sqrt 5)) 2))

(define (exact-zero? x)
  (and (number? x) (exact? x) (= x 0)))

(define (exact-one? x)
  (and (number? x) (exact? x) (= x 1)))

(define :ln2 (log 2.0))
(define :ln10 (log 10.0))

(define :minlog -1000.0)

(define *no-rationals-in-divide* #f)

(define :c 299792458)
