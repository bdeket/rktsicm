#lang racket/base

(require rackunit
         "../../kernel/express.rkt")

(define the-tests
  (test-suite
   "kernel/express"
   (test-case
    "abstract-quantity-properties"
    (define M (make-numerical-literal 'm))
    (check-equal? ((has-property? 'expression) M) 'm)
    (check-equal? ((has-property? 'expression) 'S) 'S)
    (check-equal? (get-property M 'expression) 'm)
    (check-equal? (get-property 'S 'expression) 'S)
    (check-equal? ((has-property? 'other) M) #f)
    (check-equal? (get-property M 'other 'default) 'default)
    (check-exn #px"Symbols have only EXPRESSION" (λ ()((has-property? 'other) 'S)))
    (check-equal? (get-property 'S 'other 'default) 'default)
    (add-property! M 'other #t)
    (check-equal? ((has-property? 'other) M) #t)
    (check-equal? (get-property M 'other 'default) #t)
    (check-true ((has-property? 'real) (make-real-literal 'M)))
    )))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))