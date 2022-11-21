#lang racket/base

(require rackunit
         "../../general/permute.rkt")

(define tests
  (test-suite
   "general/permute"
   (test-case "combinations"
              (check-equal? (combinations '(a b c d e) 3)
                            '((a b c)(a b d)(a b e)(a c d)(a c e)
                                     (a d e)(b c d)(b c e)(b d e)(c d e))))
   (test-case "permutation-interchanges"
              (check-equal? (permutation-interchanges '(4 2 3 1))
                            5))
   (test-case
    "sort-and-permute"
    (check-equal? (sort-and-permute
                   '(0 2 0 0 1 2 0 0) <
                   (lambda (unsorted sorted permuter unpermuter)
                     (list unsorted sorted (permuter unsorted) (unpermuter sorted))))
                  '((0 2 0 0 1 2 0 0) (0 0 0 0 0 1 2 2)
                    (0 0 0 0 0 1 2 2) (0 2 0 0 1 2 0 0))))
   (test-case "subpermute"
              (check-equal? (subpermute '((1 . 4) (4 . 2) (2 . 3) (3 . 1)) '(a b c d e))
                            '(a e d b c)))
   (test-case "factorial"
              (check-equal? (factorial 1) 1)
              (check-equal? (factorial 2) 2)
              (check-equal? (factorial 12) 479001600))
   (test-case "number-of-combinations"
              (check-equal? (number-of-combinations 1000000 12)
                            2087537916209397485013453738892186349699824088113864639583250000))))

(module+ test
  (require rackunit/text-ui)
  (run-tests tests))