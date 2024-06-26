#lang racket/base

(require rackunit
         "../../main.rkt"
         "../helper.rkt"
         )

(provide the-tests)
(define the-tests
  (test-suite
   "calculus/ode - TODO: no tests"
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))