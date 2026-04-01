#lang racket/base

(require rackunit
         "../../poly/interp.rkt"
         "../../simplify/pcf.rkt"
         "../helper.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "poly/interp"
   (test-case
    "lagrange-interpolation-function" ;; numerical version
    (define xs '(1 2 3 4 5))
    (define ys '( 2 4 8 6 3))
    (define P (lagrange-interpolation-function ys xs))
    (for ([x (in-list xs)]
          [y (in-list ys)])
      (check-equal? (P x) y))
    (check-equal? (P 3/2) 223/128))
   (test-case
    "random test"
    (define N (+ 2 (random 10)))
    (define xs (let lp ([prev 0][i 0])
                 (if (< i N)
                     (let ([nxt (+ prev 1 (random 5))]) (cons nxt (lp nxt (+ i 1))))
                     '())))
    (define ys (build-list N (λ (_) (- (random 800) 400))))
    (define P (lagrange-interpolation-function ys xs))
    (for ([x (in-list xs)]
          [y (in-list ys)])
      (check-equal? (P x) y)))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))