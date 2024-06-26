#lang racket/base

(require rackunit
         "../../../numerics/roots/multidimensional.rkt"
         "../../helper.rkt"
         )

(define 2pi (* 2 (angle -1)))

(provide the-tests)
(define the-tests
  (test-suite
   "numerics/roots/multidimensional"
   (test-case
    "1"
    (check-within (multidimensional-root 
                   (lambda (v)
                     (let ((x (vector-ref v 0)) (y (vector-ref v 1)))
                       (vector (+ x y -3) (+ x (- y) -1))))
                   (vector 1.5 1.5)
                   .01
                   1e-10)
                  #(2. 1.)
                  1e-15))
   (test-case
    "2"
    (check-within (multidimensional-root 
                   (lambda (v)
                     (let ((x (vector-ref v 0)) (y (vector-ref v 1)))
                       (vector (* x x) (+ x (- y) -1))))
                   (vector 1.5 1.5)
                   .01
                   1e-10)
                  #(1.194318926912262e-10 -.9999999998805681)
                  1e-10))
   (test-case
    "3"
    (check-within (multidimensional-root 
                   (lambda (v)
                     (let ((x (vector-ref v 0)) (y (vector-ref v 1)))
                       (vector (+ x (- y) -1) (expt (+ x y -3) 2))))
                   (vector 1.5 1.4)
                   .01
                   1e-15)
                  #(2 1)
                  1e-15))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))