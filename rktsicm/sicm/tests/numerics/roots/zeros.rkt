#lang racket/base

(require rackunit
         "../../../numerics/roots/zeros.rkt"
         "../../helper.rkt"
         )

(define 2pi (* 2 (angle -1)))

(define the-tests
  (test-suite
   "numerics/roots/zeros"
   (test-case
    "1"
    (define (kepler ecc m)
      (false-position-search
       (lambda (e)
         (- e (* ecc (sin e)) m))
       0.0
       2pi
       1e-15))
    (check-= (kepler .99 .01)
             .3422703164917599
             1e-15))
   (test-case
    "2"
    (define (kepler ecc m)
      (newton-with-false-position-search
       (lambda (e cont)
         (cont (- e (* ecc (sin e)) m)
               (- 1 (* ecc (cos e)))))
       0.0
       2pi
       1e-15))
    (check-= (kepler .99 .01)
             .34227031649177553
             1e-15)
    )
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))