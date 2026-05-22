#lang racket/base

(require rackunit
         "../../simplify/rules.rkt"
         "../helper.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "simplify/rule-simplifier"
   
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))