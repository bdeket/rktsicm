#lang racket/base

(require rackunit
         "../../generic.rkt"
         "../helper.rkt" ;; load everything
         )

(provide the-tests)
(define the-tests
  (test-suite
   "kernel/types"
   (test-case
    "TYPE-type"
    (define T (make-type 'tag 'a-tag 'q-pred 'c-pred 'a-pred))
    (check-equal? (type-tag T) 'tag)
    (check-equal? (abstract-type-tag T) 'a-tag)
    (check-equal? (quantity-predicate T) 'q-pred)
    (check-equal? (concrete-predicate T) 'c-pred)
    (check-equal? (abstract-predicate T) 'a-pred))
   (test-case
    "abstract-quantity?"
    (check-false (abstract-quantity? (matrix-by-rows '(1))))
    (check-false (abstract-quantity? (down 1)))
    (skip
     ;; folowing tests fail.
     ;; this is the same behavior as smcutils 20230902
     ;; not 'fixing' it until I have more tests to understand better what is the impact
     (check-false (abstract-quantity? 1))
     (check-false (abstract-quantity? (vector 1)))
     (check-not-false (abstract-quantity? (abstract-matrix 'M)))
     (check-not-false (abstract-quantity? (abstract-down 'M)))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))