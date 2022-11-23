#lang racket/base

(require rackunit
         "../../general/list-utils.rkt")

(define the-tests
  (test-suite
   "general/list-utils"
   (test-case
    "fringe"
    (check-equal? ((fringe-smaller-than? 3) '())
                  0)
    (check-equal? ((fringe-smaller-than? 100) '(a (b c) d))
                  4)
    (check-equal? ((fringe-smaller-than? 3) '(a (b c) d))
                  #f))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))