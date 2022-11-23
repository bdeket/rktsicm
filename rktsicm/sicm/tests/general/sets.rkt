#lang racket/base

(require rackunit
         "../../general/sets.rkt")

(define the-tests
  (test-suite
   "general/sets"
   (test-case
    ""
    (check-equal? ((set->list symbols)
                   ((union-sets symbols)
                    ((list->set symbols) '(a c e))
                    ((list->set symbols) '(d e f))))
                  '(a c d e f)))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))