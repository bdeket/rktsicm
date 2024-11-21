#lang racket/base

(require rackunit
         "../../general/permute.rkt")

(provide the-tests)
(define the-tests
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
                            2087537916209397485013453738892186349699824088113864639583250000))
   ;*************
   (test-case "permutations"
              (check-equal? (permutations '()) '(()))
              (check-equal? (permutations '(a)) '((a)))
              (check-equal? (permutations '(a b)) '((a b)(b a)))
              (check-equal? (permutations '(a b c))
                            '((a b c) (a c b) (b a c) (b c a) (c a b) (c b a))))
   (test-case "combinations"
              (check-equal? (combinations '() 1) '())
              (check-equal? (combinations '(a) 1) '((a)))
              (check-equal? (combinations '(a b) 1) '((a)(b)))
              (check-equal? (combinations '(a b) 2) '((a b)))
              (check-equal? (combinations '(a b c) 2) '((a b)(a c)(b c))))
   (test-case "list-interchanges"
              (check-equal? (list-interchanges '(1 2) '(1 2)) 0)
              (check-equal? (list-interchanges '(1 2) '(2 1)) 1)
              (check-equal? (list-interchanges '(1 2 3) '(3 2 1)) 3))
   (test-case "split-permutations"
              (check-equal? (split-permutations '(0 1) (permutations '(0 1)) list)
                            '(((0 1)) ((1 0))))
              (check-equal? (split-permutations '(a b c) (permutations '(a b c)) list)
                            '(((c a b) (b c a) (a b c)) ((c b a) (b a c) (a c b)))))
   (test-case "pertutation-interchanges"
              (check-equal? (permutation-interchanges '(0 1 2)) 0)
              (check-equal? (permutation-interchanges '(0 2 1.5)) 1)
              (check-equal? (permutation-interchanges '(2 0 1.5)) 2))
   (test-case "permute"
              (check-equal? (permute '(2 1 0) '(c b a)) '(a b c)))
   (test-case "sort-and-permute"
              (let-values ([(org srt p-fct o-fct) (sort-and-permute '(5 2 8 9) < values)])
                (check-equal? org '(5 2 8 9))
                (check-equal? srt '(2 5 8 9))
                (check-equal? (p-fct org) srt)
                (check-equal? (o-fct srt) org)
                (check-equal? (p-fct '(b a c d)) '(a b c d))))
   (test-case "factorial"
              (check-equal? (factorial 3) 6))
   (test-case "number-of-permutations"
              (check-equal? (number-of-permutations 8) (factorial 8)))
   (test-case "exact-quotient"
              (check-equal? (exact-quotient 8 2) 4)
              (check-exn #px"^assertion failed: \\(= 0 r\\)$" (λ () (exact-quotient 8 3))))
   (test-case "binomial-coefficient"
              (check-equal? (binomial-coefficient 0 0) 1)
              (check-equal? (binomial-coefficient 10 0) 1)
              (check-equal? (binomial-coefficient 10 3) 120))
   (test-case "number-of-combinations"
              (check-equal? (number-of-combinations 5 0) 1)
              (check-equal? (number-of-combinations 6 7) 0)
              (check-equal? (number-of-combinations 6 6) 1)
              (check-equal? (number-of-combinations 10 3) (binomial-coefficient 10 3))
              (check-equal? (number-of-combinations 10 4) (binomial-coefficient 10 4)))
   (test-case "permutation-parity"
              (check-equal? (permutation-parity '(c b a) '(a b c)) -1)
              (check-equal? (permutation-parity '(b c a) '(a b c))  1)
              (check-equal? (permutation-parity '(c b a) '(3 2 1))  0))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))