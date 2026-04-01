#lang racket/base

(require rackunit
         "../../poly/hermite.rkt"
         "../../simplify/pcf.rkt"
         "../helper.rkt")

(define (poly:from-roots #:a [a 1] . lst)
  (for/fold ([P (poly:dense-> (list a))])
            ([r (in-list lst)])
    (poly:* P (poly:dense-> (list (- r) 1)))))

(provide the-tests)
(define the-tests
  (test-suite
   "poly/hermite"
   (test-case
    "make-hermite-interpolator"
    ;; up till 0'th derivative
    (define H0 (make-hermite-interpolator 0))
    (check-equal? (H0 (list 0 (sin 0)) (list 1 (sin 1))) (poly:dense-> '(0 0.8414709848078965)))
    ;; up till 2nd derivative
    (define P (poly:dense-> '(2 4 8 3)))
    (define P*1 (poly:dense-> '(4 16 9)))
    (define P*2 (poly:dense-> '(16 18)))
    (define H2 (make-hermite-interpolator 2))
    (check-equal? (H2 (list 0 (poly:value P 0) (poly:value P*1 0) (poly:value P*2 0))
                      (list 3 (poly:value P 3) (poly:value P*1 3) (poly:value P*2 3)))
                  P)
    )
   (test-case
    "make-quintic-interpolant"
    (define P (poly:dense-> '(2 4 8 3)))
    (define P*1 (poly:dense-> '(4 16 9)))
    (define P*2 (poly:dense-> '(16 18)))
    ;; specialised H2
    (check-equal? (make-quintic-interpolant 0 (poly:value P 0) (poly:value P*1 0) (poly:value P*2 0)
                                            3 (poly:value P 3) (poly:value P*1 3) (poly:value P*2 3))
                  P))
   (test-case
    "make-cubic-interpolant"
    (define P (poly:dense-> '(2 4 8 3)))
    (define P*1 (poly:dense-> '(4 16 9)))
    (define P*2 (poly:dense-> '(16 18)))
    ;; specialised H1
    (check-equal? (make-cubic-interpolant 0 (poly:value P 0) (poly:value P*1 0)
                                          3 (poly:value P 3) (poly:value P*1 3))
                  P))
   (test-case
    "make-cubic-interpolant - insufficient"
    (define P (poly:dense-> '(2 4 8 3 6 -1)))
    (define P*1 (poly:dense-> '(4 16 9 24 -5)))
    ;; specialised H1
    (check-equal? (make-cubic-interpolant 0 (poly:value P 0) (poly:value P*1 0)
                                          3 (poly:value P 3) (poly:value P*1 3))
                  (poly:dense-> '(2 4 8 12))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))