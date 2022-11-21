#lang racket/base

(require rackunit
         "../../main.rkt"
         "../helper.rkt"
         )

(define the-tests
  (test-suite
   "calculus/frame-maker - TODO: no tests"
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))