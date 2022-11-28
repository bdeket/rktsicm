#lang racket/base

(require rackunit
         "../../../numerics/linear/vandermonde.rkt"
         "../../../kernel-intr.rkt"
         )


(define the-tests
  (test-suite
   "numerics/linear/vandermonde"
   (check-equal? (solve-vandermonde-system '(3 5 7) '(9 11 13)
			  (lambda (x) x) (lambda () 'foo))
                 '(6 1 0))
   (check-equal? (solve-vandermonde-t-system '(3 5 7) '(9 11 13)
			    (lambda (x) x) (lambda () 'foo))
                 '(49/2 -23 15/2))
   (check-equal? (solve-vandermonde-td-system '(3 5 0) '(9 11 13)
			     (lambda (x) x) (lambda () 'foo))
                 'foo)
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))