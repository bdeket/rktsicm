#lang racket/base

(require rackunit
         "../../simplify/rule-simplifier.rkt"
         "../../simplify/syntax.rkt"
         "../helper.rkt")


(provide the-tests)
(define the-tests
  (test-suite
   "simplify/syntax"
   (test-case
    "try-rules"
    (define rules
      (rule-system [(+ (? a number?) (?? bs))
                    (apply + a bs)]
                   [(+ (? a) (?? bs))
                    none
                    (rkt:+ (: a) (:: bs))]
                   [(* (?? as))
                    (andmap number? as)
                    something-else]))
    (check-equal? (rules '(+ 1 2 3)) 6)
    (check-equal? (rules '(+ one 2 3)) '(rkt:+ one 2 3))
    (check-equal? (rules '(* a 3)) '(* a 3))
    (check-equal? (rules '(* 2 3)) 'something-else))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))