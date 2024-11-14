#lang racket/base

(require rackunit
         "../../general/resource-limit.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "general/resource-limit"
   (test-case
    "with-limited-time / allocated-time-expired"
    (check-equal? (with-limited-time +inf.0
                                     (λ () (if (allocated-time-expired?) 'expired 'continue)))
                  'continue)
    (check-equal? (with-limited-time 0.0
                                     (λ () (sleep .001) (if (allocated-time-expired?) 'expired 'continue)))
                  'expired))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))