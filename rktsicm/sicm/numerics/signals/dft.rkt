#lang racket/base

(provide (all-defined-out))

(require "../../kernel-intr.rkt"
         "../../general/assert.rkt"
         "../../general/list-utils.rkt"
         "fft.rkt"
         )

;;; Assume we have loaded the dumb fft program.

(define (fft x)
  (map heuristic-round-complex
       ((cadr (make-transform-pair (length x))) x)))

(define (ifft x)
  (map heuristic-round-complex
       ((caddr (make-transform-pair (length x))) x)))

#|
(fft (list 1 0 0 0 0 0 0 0))
;Value: (1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8)

(fft (list 1 1 1 1 1 1 1 1))
;Value: (1 0 0 0 0 0 0 0)

(ifft (fft (list 1 1 1 1 1 1 1 1)))
;Value: (1 1 1 1 1 1 1 1)

(define (m-cycles-cos-in-n-samples m n)
  (let ((w (/ (* 2pi m) n)))
    (build-list
     n
     (lambda (i)
       (cos (* w i))))))
(fft (m-cycles-cos-in-n-samples 0 8))
;Value: (1 0 0 0 0 0 0 0)

(fft (m-cycles-cos-in-n-samples 1 8))
;Value: (0 1/2 0 0 0 0 0 1/2)

(fft (m-cycles-cos-in-n-samples 2 8))
;Value: (0 0 1/2 0 0 0 1/2 0)

(fft (m-cycles-cos-in-n-samples 3 8))
;Value: (0 0 0 1/2 0 1/2 0 0)

(fft (m-cycles-cos-in-n-samples 4 8))
;Value: (0 0 0 0 1 0 0 0)

(define (m-cycles-sin-in-n-samples m n)
  (let ((w (/ (* 2pi m) n)))
    (build-list
     n
     (lambda (i)
       (sin (* w i))))))

(fft (m-cycles-sin-in-n-samples 0 8))
;Value: (0 0 0 0 0 0 0 0)

(fft (m-cycles-sin-in-n-samples 1 8))
;Value: (0 -1/2i 0 0 0 0 0 +1/2i)

(fft (m-cycles-sin-in-n-samples 2 8))
;Value 71: (0 0 +1/2i 0 0 0 -1/2i 0)

(fft (m-cycles-sin-in-n-samples 3 8))
;Value: (0 0 0 +1/2i 0 -1/2i 0 0)

(fft (m-cycles-sin-in-n-samples 4 8))
;Value: (0 0 0 0 0 0 0 0)
|#

;;; But I want both the time domain and the frequency domain to be
;;; centered so that 0 is in the middle of the sample sequence.

(define (rotate-list l n2)
  (append (list-tail l n2) (take l n2)))

(define (dft x)
  (let ((n (length x)))
    (assert (power-of-2? n))
    (let ((n2 (quotient n 2)))
      (rotate-list (fft (rotate-list x n2)) n2))))

(define (idft x)
  (let ((n (length x)))
    (assert (power-of-2? n))
    (let ((n2 (quotient n 2)))
      (rotate-list (ifft (rotate-list x n2)) n2))))

;;; Suppose we have a function that takes real arguments in the
;;; interval [-a,+a) and we want to sample that interval to make a
;;; discrete sequence of length n for the dft program:

(define (samples n f a)
  (assert (power-of-2? n))
  (map f (build-list n (λ (i)(+ (- a) (* i (/ (* 2 a) n)))))))

#|
(dft (samples 8 (lambda (x) (cos (* 0 x))) pi))
;Value: (0 0 0 0 1 0 0 0)

(dft (samples 8 (lambda (x) (cos (* 1 x))) pi))
;Value: (0 0 0 1/2 0 1/2 0 0)

(dft (samples 8 (lambda (x) (cos (* 2 x))) pi))
;Value: (0 0 1/2 0 0 0 1/2 0)

(dft (samples 8 (lambda (x) (cos (* 3 x))) pi))
;Value: (0 1/2 0 0 0 0 0 1/2)

(dft (samples 8 (lambda (x) (cos (* 4 x))) pi))
;Value: (1 0 0 0 0 0 0 0)


(dft (samples 8 (lambda (x) (sin (* 0 x))) pi))
;Value: (0 0 0 0 0 0 0 0)

(dft (samples 8 (lambda (x) (sin (* 1 x))) pi))
;Value: (0 0 0 -1/2i 0 +1/2i 0 0)

(dft (samples 8 (lambda (x) (sin (* 2 x))) pi))
;Value: (0 0 -1/2i 0 0 0 +1/2i 0)

(dft (samples 8 (lambda (x) (sin (* 3 x))) pi))
;Value: (0 -1/2i 0 0 0 0 0 +1/2i)

(dft (samples 8 (lambda (x) (sin (* 4 x))) pi))
;Value: (0 0 0 0 0 0 0 0)

;;; It even seems to work.

(define x
  (samples 64 (lambda (x)
		(- (random 1.0) 0.5))
	   pi))

(define X (dft x))

(define y (idft X))

(apply max (map - x y))
;Value: 1.3877787807814457e-16
|#
