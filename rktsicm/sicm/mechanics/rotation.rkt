#lang s-exp "../generic.rkt"

(provide (all-defined-out))

;;;; Utilities for dealing with rotations

;;; Rotation matrices

(define (rotate-x-matrix-2 cos-angle sin-angle)
  (matrix-by-rows 
    (list           1               0              0)
    (list           0       cos-angle   (- sin-angle))
    (list           0       sin-angle      cos-angle)))

(define (rotate-x-matrix angle)
  (rotate-x-matrix-2 (cos angle) (sin angle)))

(define Rx-matrix rotate-x-matrix)

(define (rotate-y-matrix-2 cos-angle sin-angle)
  (matrix-by-rows 
   (list     cos-angle     0  sin-angle)
   (list             0     1          0)
   (list (- sin-angle)     0  cos-angle)))

(define (rotate-y-matrix angle)
  (rotate-y-matrix-2 (cos angle) (sin angle)))

(define Ry-matrix rotate-y-matrix)


(define (rotate-z-matrix-2 cos-angle sin-angle)
  (matrix-by-rows
   (list cos-angle (- sin-angle)    0)
   (list sin-angle    cos-angle     0)
   (list         0            0     1)))

(define (rotate-z-matrix angle)
  (rotate-z-matrix-2 (cos angle) (sin angle)))

(define Rz-matrix rotate-z-matrix)


(define (angle&axis->rotation-matrix theta n)
  ;; (assert (v:unit? n))
  (let ((x (ref n 0)) (y (ref n 1)) (z (ref n 2)))
    (let ((colatitude (acos z))
	  ;OK because (< colatitude :pi)
	  (longitude (atan y x)))
      (* (rotate-z-matrix longitude)
	 (rotate-y-matrix colatitude)
	 (rotate-z-matrix theta)
	 (m:transpose (rotate-y-matrix colatitude))
	 (m:transpose (rotate-z-matrix longitude))))))

;;; Rotation tuples

(define (rotate-x-tuple-2 c s)
  (m->s (down 'ignore 'ignore 'ignore)
	(rotate-x-matrix-2 c s)
	(up 'ignore 'ignore 'ignore)))

(define (rotate-x-tuple angle)
  (rotate-x-tuple-2 (cos angle) (sin angle)))

(define (rotate-y-tuple-2 c s)
  (m->s (down 'ignore 'ignore 'ignore)
	(rotate-y-matrix-2 c s)
	(up 'ignore 'ignore 'ignore)))

(define (rotate-y-tuple angle)
  (rotate-y-tuple-2 (cos angle) (sin angle)))


(define (rotate-z-tuple-2 c s)
  (m->s (down 'ignore 'ignore 'ignore)
	(rotate-z-matrix-2 c s)
	(up 'ignore 'ignore 'ignore)))

(define (rotate-z-tuple angle)
  (rotate-z-tuple-2 (cos angle) (sin angle)))

;;; Rotation procedures

(define (rotate-x-2 c s)
  (let ((tuple (rotate-x-tuple-2 c s)))
    (lambda (v) (* tuple v))))

(define (rotate-x angle)
  (rotate-x-2 (cos angle) (sin angle)))


(define (rotate-y-2 c s)
  (let ((tuple (rotate-y-tuple-2 c s)))
    (lambda (v) (* tuple v))))

(define (rotate-y angle)
  (rotate-y-2 (cos angle) (sin angle)))


(define (rotate-z-2 c s)
  (let ((tuple (rotate-z-tuple-2 c s)))
    (lambda (v) (* tuple v))))

(define (rotate-z angle)
  (rotate-z-2 (cos angle) (sin angle)))



(define (wcross->w A)
  (up (ref A 1 2)
      (ref A 2 0)
      (ref A 0 1)))

