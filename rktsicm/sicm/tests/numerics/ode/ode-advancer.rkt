#lang racket/base

(require rackunit
         "../../../numerics/ode/ode-advancer.rkt"
         "../../../kernel-intr.rkt"
         "../../helper.rkt"
         )


(define the-tests
  (test-suite
   "numerics/ode/ode-advancer"
   (check-within ((ode-advancer
                   (lambda (s) (up 1 (g:ref s 1)))
                   1.e-12
                   2)
                  (up 0 1)
                  1.0
                  list)
                 (list (up 1. 2.718281828459047) 1 1.5)
                 1e-15)
    ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))