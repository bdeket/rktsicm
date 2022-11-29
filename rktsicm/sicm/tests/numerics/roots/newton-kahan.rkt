#lang racket/base

(require rackunit
         "../../../numerics/roots/newton-kahan.rkt"
         "../../helper.rkt"
         )

(define 2pi (* 2 (angle -1)))

(define the-tests
  (test-suite
   "numerics/roots/newton-kahan"
   (test-case
    "1"
    (check-= (newton-search
              (lambda (x cont)
                (cont (cos x) (- (sin x))))
              1.0
              1e-15)
             1.5707963267948966
             1e-15))
   ;;; If the root is multiple, the convergence is much slower 
   ;;;  and much less accurate.
   (test-case
    "multi-root"
    (check-= (newton-search
              (lambda (x cont)
                (cont (- 1.0 (sin x)) (- (cos x))))
              1
              1e-15)
             1.570796319310356
             1e-15))
   (test-case
    "ntkh"
    (check-= (newton-kahan-search
              (lambda (x cont)
                (cont (cos x) (- (sin x))))
              1.0
              2.0
              1e-15)
             1.5707963267948966
             1e-15))
   (test-case
    "ntkh2"
    ;;; Kahan's method really speeds things up here, but it
    ;;;  doesn't make the result more accurate.
    (check-= (newton-kahan-search
              (lambda (x cont)
                (cont (- 1.0 (sin x)) (- (cos x))))
              1.0
              2.0
              1e-15)
             1.5707963255702555
             1e-15))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))