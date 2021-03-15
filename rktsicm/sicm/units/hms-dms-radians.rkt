#lang racket/base

(provide (all-defined-out))

(require "../kernel/numeric.rkt")

;;; Some elementary unit conversions

(define (degrees->radians degrees)
  (* (/ :2pi 360) degrees))

(define (radians->degrees radians)
  (* (/ 360 :2pi) radians))


(define (xms->x xms)
  (+ (car xms)
     (/ (cadr xms) 60)
     (/ (caddr xms) 3600)))

(define (x->xms x)
  (let* ((d (truncate x))
	 (dd (- x d))
	 (m (truncate (* 60 dd)))
	 (ddm (- dd (/ m 60)))
	 (s (* 3600 ddm)))
    (list d m s)))

(define dms->d xms->x)
(define d->dms x->xms)

(define (dms->radians dms)
  (degrees->radians (dms->d dms)))

(define (radians->dms radians)
  (d->dms (radians->degrees radians)))


(define (hours->radians hours)
  (* (/ :2pi 24) hours))

(define (radians->hours radians)
  (* (/ 24 :2pi) radians))


(define hms->h xms->x)
(define h->hms x->xms)

(define (hms->radians hms)
  (* 15 (dms->radians hms)))

(define (radians->hms radians)
  (radians->dms (/ radians 15)))
