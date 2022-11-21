#lang racket/base

(require rackunit
         "../../main.rkt"
         "../helper.rkt"
         )

(define the-tests
  (test-suite
   "calculus/SR-boost"
   (check-simplified? (proper-space-interval
                       ((general-boost (up 'vx 'vy 'vz))
                        (make-4tuple 'ct (up 'x 'y 'z))))
                      (proper-space-interval
                       (make-4tuple 'ct (up 'x 'y 'z))))
   (check-simplified? (let ((beta (up (/ 'v^x :c) (/ 'v^y :c) (/ 'v^z :c))))
                        (- ((general-boost2 (up 1 0 0) 0) (up 'u0 'u1 'u2 'u3))
                           (up 'u0 'u1 'u2 'u3)))
                      (up 0 0 0 0))
   ;;; Check of the relation between boosts and rotations.
   (let ((beta (up 'bx 'by 'bz))
         (xi (make-4tuple 'ct (up 'x 'y 'z)))
         (R (compose
             (rotate-x 'theta)
             (rotate-y 'phi)
             (rotate-z 'psi)))
         (R-inverse (compose
                     (rotate-z (- 'psi))
                     (rotate-y (- 'phi))
                     (rotate-x (- 'theta)))))
     (check-simplified? ((general-boost beta) xi)
                        ((compose (extended-rotation R-inverse)
                                  (general-boost (R beta))
                                  (extended-rotation R))
                         xi)
                        #:timeout 3))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))