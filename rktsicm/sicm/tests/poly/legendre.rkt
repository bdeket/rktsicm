#lang racket/base

(require rackunit
         "../../poly/legendre.rkt"
         "../../simplify/pcf.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "poly/legendre"
   (test-case
    "legendre-polynomials"
    (check-equal? (legendre-polynomial 0) (poly:dense-> '(1)))
    (check-equal? (legendre-polynomial 1) (poly:dense-> '(0 1)))
    (check-equal? (legendre-polynomial 2) (poly:dense-> '(-1/2 0 3/2)))
    (check-equal? (legendre-polynomial 3) (poly:dense-> '(0 -3/2 0 5/2)))
    (check-equal? (legendre-polynomial 4) (poly:dense-> '(3/8 0 -15/4 0 35/8)))
    )
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))