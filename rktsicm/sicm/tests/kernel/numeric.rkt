#lang racket/base

(require rackunit
         "../../main.rkt"
         "../../rkt/int.rkt"
         )

(define kernel-tests
  (test-suite
   "kernel/numeric"
   (test-case "ORIG:stirling-first-kind"
              (check-equal? (stirling-first-kind 1 1) 1)
              (check-equal? (stirling-first-kind 2 1) 1)
              (check-equal? (stirling-first-kind 2 2) 1)
              (check-equal? (stirling-first-kind 3 1) 2)
              (check-equal? (stirling-first-kind 3 2) 3)
              (check-equal? (stirling-first-kind 5 2) 50)
              (check-equal? (stirling-first-kind 7 3) 1624)
              )

   (test-case "ORIG:stirling-second-kind"
              (check-equal? (stirling-second-kind 5 3) 25)
              )

   (test-case "ORIG:sigma"
              (check-equal? (sigma (lambda (x) (/ 1.0 x)) 1 10000000)
                            16.695311365857272))

   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests kernel-tests))