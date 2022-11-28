#lang racket/base

(require rackunit
         "../../../numerics/optimize/optimize.rkt"
         "../../../kernel-intr.rkt"
         )


(define the-tests
  (test-suite
   "numerics/optimize/optimize"
   (check-equal? (parameters->vector
                  (list (vector 1 2.3 4)
                        (up 3.5 (down 5 1.3) 6)))
                 #(1 2.3 4 3.5 5 1.3 6))
   (check-equal? ((vector->parameters
                   (list (vector 'a 'b 'c) (up 'd (down 'e 'f) 'g)))
                  #(1 2.3 4 3.5 5 1.3 6))
                 (list #(1 2.3 4) (up 3.5 (down 5 1.3) 6)))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))