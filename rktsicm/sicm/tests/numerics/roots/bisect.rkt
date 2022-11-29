#lang racket/base

(require rackunit
         "../../../numerics/roots/bisect.rkt"
         "../../helper.rkt"
         )


(define the-tests
  (test-suite
   "numerics/roots/bisect"
   (test-case
    "bisect-1"
    (define (kepler ecc m)
      (bisect-1
       (lambda (e)
         (- e (* ecc (sin e)) m))
       0.0
       (* 2 (angle -1))))
    (check-= (kepler .99 .01)
              .3422703164917749
              1e-15))
   (test-case
    "bisect-2"
    (define (kepler ecc m)
      (bisect-2
       (lambda (e)
         (- e (* ecc (sin e)) m))
       0.0
       (* 2 (angle -1))
       1e-15))
    (check-= (kepler .99 .01)
             .3422703164917749
             1e-15))
   (test-case
    "bisect-fp"
    (define (kepler ecc m)
      (bisect-fp
       (lambda (e)
         (- e (* ecc (sin e)) m))
       0.0
       (* 2 (angle -1))
       1e-15))
    (check-= (kepler .99 .01)
             .342270316491775
             1e-15))
   (test-case
    "bisect"
    (define (kepler ecc m)
      (bisect
       (lambda (e)
         (- e (* ecc (sin e)) m))
       0.0
       (* 2 (angle -1))
       1e-15
       20))
    (check-= (kepler .99 .01)
             .34227031649177553
             1e-15))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))