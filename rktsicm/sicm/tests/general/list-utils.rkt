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
   (test-case
    "lset-union"
    (check-true (lset= eq? (lset-union eq? '(1 2 3) '(3 4 5))
                       '(1 2 3 4 5))))
   (test-case
    "lset-difference"
    (check-equal? (lset-difference eq? (list (vector)) (list (vector)))
                  (list (vector)))
    (check-equal? (lset-difference equal? '(1 2 3 4 5 6) '(3 4 5))
                  '(1 2 6))
    (check-equal? (lset-difference eq? '(1 2 3) '(1 2 3))
                  '())
    (check-equal? (lset-difference eq? '(1 2 3) '())
                  '(1 2 3))
    (check-equal? (lset-difference eq? '() '(1 2 3))
                  '()))
   (test-case
    "lset-intersection"
    (check-equal? (lset-intersection eq? '(1 2 3 4 5) '(4 5 6))
                  '(4 5))
    (check-equal? (lset-intersection eq? '(1 2 3) '(4 5 6))
                  '()))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))