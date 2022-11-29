#lang racket/base

(require rackunit
         "../../../numerics/signals/dft.rkt"
         "../../../numerics/signals/fft.rkt"
         racket/list
         )

(define pi (angle -1))
(define 2pi (* 2 pi))

(define the-tests
  (test-suite
   "numerics/signals/fft"
   (check-within (fft '(1 0 0 0 0 0 0 0))
                 (make-list 8 1/8)
                 1e-15)
   (check-within (fft '(1 1 1 1 1 1 1 1))
                 (cons 1 (make-list 7 0))
                 1e-15)
   (check-within (ifft (fft (list 1 1 1 1 1 1 1 1)))
                 (make-list 8 1)
                 1e-15)
   (test-case
    "cos-sampler"
    (define (m-cycles-cos-in-n-samples m n)
      (let ((w (/ (* 2pi m) n)))
        (build-list
         n
         (lambda (i)
           (cos (* w i))))))
    (check-within (fft (m-cycles-cos-in-n-samples 0 8))
                  (cons 1 (make-list 7 0))
                  1e-15)
    (check-within (fft (m-cycles-cos-in-n-samples 1 8))
                  '(0 1/2 0 0 0 0 0 1/2)
                  1e-15)
    (check-within (fft (m-cycles-cos-in-n-samples 2 8))
                  '(0 0 1/2 0 0 0 1/2 0)
                  1e-15)
    (check-within (fft (m-cycles-cos-in-n-samples 3 8))
                  '(0 0 0 1/2 0 1/2 0 0)
                  1e-15)
    (check-within (fft (m-cycles-cos-in-n-samples 4 8))
                  '(0 0 0 0 1 0 0 0)
                  1e-15))
   (test-case
    "sin-sampler"
    (define (m-cycles-sin-in-n-samples m n)
      (let ((w (/ (* 2pi m) n)))
        (build-list
         n
         (lambda (i)
           (sin (* w i))))))
    (check-within (fft (m-cycles-sin-in-n-samples 0 8))
                  '(0 0 0 0 0 0 0 0)
                  1e-15)
    (check-within (fft (m-cycles-sin-in-n-samples 1 8))
                  '(0 +1/2i 0 0 0 0 0 -1/2i)
                  1e-15)
    (check-within (fft (m-cycles-sin-in-n-samples 2 8))
                  '(0 0 +1/2i 0 0 0 -1/2i 0)
                  1e-15)
    (check-within (fft (m-cycles-sin-in-n-samples 3 8))
                  '(0 0 0 +1/2i 0 -1/2i 0 0)
                  1e-1)
    (check-within (fft (m-cycles-sin-in-n-samples 4 8))
                  '(0 0 0 0 0 0 0 0)
                  1e-15))
   (test-case
    "dft"
    (check-within (dft (samples 8 (lambda (x) (cos (* 0 x))) pi))
                  '(0 0 0 0 1 0 0 0)
                  1e-15)
    (check-within (dft (samples 8 (lambda (x) (cos (* 1 x))) pi))
                  '(0 0 0 1/2 0 1/2 0 0)
                  1e-15)
    (check-within (dft (samples 8 (lambda (x) (cos (* 2 x))) pi))
                  '(0 0 1/2 0 0 0 1/2 0)
                  1e-15)
    (check-within (dft (samples 8 (lambda (x) (cos (* 3 x))) pi))
                  '(0 1/2 0 0 0 0 0 1/2)
                  1e-15)
    (check-within (dft (samples 8 (lambda (x) (cos (* 4 x))) pi))
                  '(1 0 0 0 0 0 0 0)
                  1e-15)
    (check-within (dft (samples 8 (lambda (x) (sin (* 0 x))) pi))
                  '(0 0 0 0 0 0 0 0)
                  1e-15)
    (check-within (dft (samples 8 (lambda (x) (sin (* 1 x))) pi))
                  '(0 0 0 -1/2i 0 +1/2i 0 0)
                  1e-15)
    (check-within (dft (samples 8 (lambda (x) (sin (* 2 x))) pi))
                  '(0 0 -1/2i 0 0 0 +1/2i 0)
                  1e-15)
    (check-within (dft (samples 8 (lambda (x) (sin (* 3 x))) pi))
                  '(0 -1/2i 0 0 0 0 0 +1/2i)
                  1e-15)
    (check-within (dft (samples 8 (lambda (x) (sin (* 4 x))) pi))
                  '(0 0 0 0 0 0 0 0)
                  1e-15))
   (test-case
    "+"
;;; It even seems to work.
    (define x
      (samples 64 (lambda (x) (- (random) 0.5)) pi))
    (define X (dft x))
    (define y (idft X))
    (check-= (apply max (map - x y)) 0 1e-15)
    )
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))