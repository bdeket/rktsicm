#lang racket/base

(require rackunit
         "../../poly/domain.rkt"
         "../../simplify/pcf.rkt"
         "../helper.rkt")

(define (poly:from-roots #:a [a 1] . lst)
  (for/fold ([P (poly:dense-> (list a))])
            ([r (in-list lst)])
    (poly:* P (poly:dense-> (list (- r) 1)))))

(provide the-tests)
(define the-tests
  (test-suite
   "poly/domain"
   (test-case
    "poly-domain->canonical"
    (check-equal? (poly-domain->canonical (poly:from-roots 0 1) 0 1) (poly:from-roots #:a 1/4 -1 1))
    (check-equal? (poly-domain->canonical (poly:from-roots 4 10 12) 4 12) (poly:from-roots #:a 64 -1 1/2 1))
    (check-equal? (poly:value (poly:from-roots 4 10 12) 8)
                  (poly:value (poly:from-roots #:a 64 -1 1/2 1) 0))
    (check-equal? (poly:value (poly:from-roots 4 10 12) 6)
                  (poly:value (poly:from-roots #:a 64 -1 1/2 1) -1/2))
    (check-exn #px"bad interval: must have a < b in POLY-DOMAIN->CANONICAL" (λ () (poly-domain->canonical 1 1 0))))
   (test-case
    "poly-domain->general"
    (check-equal? (poly-domain->general (poly:from-roots #:a 1/4 -1 1) 0 1) (poly:from-roots 0 1))
    (check-equal? (poly-domain->general (poly:from-roots #:a 64 -1 1/2 1) 4 12) (poly:from-roots 4 10 12))
    (check-exn #px"bad interval: must have a < b in POLY-DOMAIN->GENERAL" (λ () (poly-domain->general 1 1 0))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))