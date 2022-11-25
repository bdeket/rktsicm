#lang s-exp "../../kernel.rkt"

(require rackunit
         "../../rkt/glue.rkt"
         "../../general/assert.rkt"
         "../../simplify.rkt"
         "../helper.rkt")

(define (gcd-test d f g)
      (let ((pd (fpf:expression-> d (lambda (p v) p)))
            (pf (fpf:expression-> f (lambda (p v) p)))
            (pg (fpf:expression-> g (lambda (p v) p))))
        (let ((pdf (fpf:* pd pf)) (pdg (fpf:* pd pg)))
          (sparse-gcd (fpf:->sparse pdf) (fpf:->sparse pdg)
                      (lambda (g)
                        (if (equal? (sort g sparse-term->)
                                    (fpf:->sparse pd))
                            #t
                            (println (list g (fpf:->sparse pd)))))
                      (lambda () #f)))))

(define the-tests
  (test-suite
   "simplify/sparse"
   (check-equal? (sparse-univariate-gcd
                  '(((8) . 1) ((6) . 1) ((4) . -3) ((3) . -3) ((2) . 8) ((1) . 2) ((0) . -5))
                  '(((6) . 3) ((4) . 5) ((2) . -4) ((1) . -9) ((0) . 21)))
                 '(((0) . 1)))
   ;;Test repaired by gjs on 16 Aug 2021
   (test-case
    "1"
    
    (define d1 '(+ (expt x1 2) x1 3))
    (define f1 '(+ (* 2 (expt x1 2)) (* 2 x1) 1))
    (define g1 '(+ (expt x1 2) (* 2 x1) 2))
    (check-true (gcd-test d1 f1 g1)))
   (test-case
    "2"
    (define d2
      '(+ (* 2 (expt x1 2) (expt x2 2))
          (* x1 x2)
          (* 2 x1)))
    (define f2
      '(+ (expt x2 2)
          (* 2 (expt x1 2) x2)
          (expt x1 2)
          1))
    (define g2
      '(+ (* (expt x1 2) (expt x2 2))
          (* (expt x1 2) x2)
          (* x1 x2)
          (expt x1 2)
          x1))
    (check-true (gcd-test d2 f2 g2)))
   (test-case
    "3"
    (define d3
      '(+ (* x2 x2 x3 x3)
          (* x2 x2 x3)
          (* 2 x1 x1 x2 x3)
          (* x1 x3)))
    (define f3
      '(+ (* x3 x3)
          (* x2 x2 x3)
          (* x1 x1 x2 x3)
          (* x1 x3)
          (* x1 x1 x2 x2)))
    (define g3
      '(+ (* x2 x3)
          (* 2 x1 x3)
          x3
          x1))
    (check-true (gcd-test d3 f3 g3)))
   (test-case
    "4"
    (define d4
      '(+ (* x1 x1 x4 x4)
          (* x2 x2 x3 x4)
          (* x1 x1 x2 x4)
          (* x2 x4)
          (* x1 x1 x2 x3)))
    (define f4
      '(+ (* x1 x2 x3 x3 x4 x4)
          (* x1 x3 x3 x4 x4)
          (* x1 x4 x4)
          (* x4 x4)
          (* x1 x3 x4)))
    (define g4
      '(+ (* x1 x3 x3 x4 x4)
          (* x3 x3 x4 x4)
          (* x4 x4)
          (* x1 x2 x2 x3 x4)
          (* x1 x2 x2)))
    (check-true (gcd-test d4 f4 g4)))
   (test-case
    "5"
    (define d5
      '(+ (* x1 x1 x1 x2 x2 x3 x3 x4 x5 x5)
          (* x1 x2 x2 x5 x5)
          (* x1 x1 x1 x3 x4 x4 x5)
          (* x1 x1 x1 x2 x3 x3 x4 x5)
          (* x1 x1 x2 x3 x3 x4 x4)))
    (define f5
      '(+ (* x1 x2 x2 x5 x5)
          (* x1 x2 x3 x3 x4 x5)
          (* x1 x2 x3 x3 x4 x4)
          (* x1 x2 x2 x4 x4)
          1))
    (define g5
      '(+ (* x1 x3 x3 x4 x5 x5)
          (* x2 x5 x5)
          (* x1 x2 x4 x5)
          (* x2 x5)
          (* x1 x2 x3 x4 x4)))
    (check-true (gcd-test d5 f5 g5)))
   (test-case
    "6"
    (define d6
      '(+ (* x1 x2 x4 x4 x5 x5 x6 x6)
          (* x1 x2 x2 x3 x3 x4 x5 x5 x6 x6)
          (* x1 x1 x3 x6 x6)
          (* x1 x1 x2 x3 x3 x4 x5 x5 x6)
          (* x1 x1 x3 x5 x6)))
    (define f6
      '(+ (* x1 x1 x2 x4 x5 x5 x6 x6)
          (* x1 x3 x5 x5 x6 x6)
          (* x1 x2 x2 x6 x6)
          (* x1 x1 x2 x2 x3 x3 x5 x6)
          (* x1 x3 x3 x4 x5)))
    (define g6
      '(+ (* x2 x2 x3 x3 x4 x5 x5 x6)
          (* x1 x4 x4 x5 x6)
          (* x2 x2 x3 x3 x4 x5 x6)
          (* x1 x2 x2 x3 x4 x4 x6)
          (* x1 x1 x3 x5 x5)))
    (check-true (gcd-test d6 f6 g6)))
   (test-case
    "7"
    (define d7
      '(+ (* x1 x2 x2 x4 x4 x6 x6 x7 x7)
          (* x1 x1 x3 x4 x6 x6 x7 x7)
          (* x3 x3 x4 x4 x7 x7)
          (* x1 x1 x2 x4 x4 x6)
          (* x3 x4 x5 x5)))
    (define f7
      '(+ (* x1 x1 x2 x4 x4 x5 x6 x6 x7 x7)
          (* x1 x2 x3 x6 x7)
          (* x3 x4 x4 x5 x5 x7)
          (* x1 x1 x2 x3 x4 x4 x5 x6)))
    (define g7
      '(+ (* x1 x3 x5 x6 x6 x7 x7)
          (* x2 x2 x3 x3 x4 x4 x5 x6 x7 x7)
          (* x4 x6 x7 x7)
          (* x1 x1 x2 x3 x5 x6 x7)
          (* x1 x1 x3 x3 x4 x5 x5)))
    (check-true (gcd-test d7 f7 g7)))
   (test-case
    "8"
    (define d8
      '(+ (* x2 x2 x4 x5 x6 x7 x8 x8)
          (* x1 x1 x2 x3 x3 x4 x4 x6 x6 x7 x7 x8)
          (* x1 x1 x3 x4 x4 x6 x6 x7 x7)
          (* x1 x1 x2 x2 x3 x3 x4 x5 x5 x6 x7 x7)
          (* x2 x2 x4 x6)))
    (define f8
      '(+ (* x1 x1 x2 x2 x3 x4 x4 x5 x6 x6 x8 x8)
          (* x2 x5 x6 x6 x8 x8)
          (* x1 x1 x2 x2 x3 x3 x4 x4 x6 x6 x7 x7 x8)
          (* x1 x1 x3 x3 x4 x5 x5 x7 x7 x8)
          (* x1 x2 x2 x3 x3 x5 x5 x7)))
    (define g8
      '(+ (* x1 x4 x4 x6 x6 x7 x8 x8)
          (* x1 x2 x2 x4 x4 x5 x5 x6 x6 x8)
          (* x1 x1 x2 x3 x4 x4 x6 x6 x8)
          (* x1 x1 x2 x2 x3 x3 x4 x5 x5 x8)
          (* x1 x2 x4 x4 x5 x5)))
    (check-true (gcd-test d8 f8 g8)))
   (test-case
    "10"
    (define d10
      '(+ (* x1 x2 x2 x4 x4 x8 x9 x9 x10 x10)
          (* x2 x2 x4 x5 x5 x6 x7 x9 x10 x10)
          (* x1 x1 x2 x3 x5 x5 x7 x7 x9 x9)
          (* x1 x3 x3 x4 x4 x7 x7 x9 x9)
          (* x1 x1 x3 x4 x7 x7 x8 x8)))
    (define f10
      '(+ (* x1 x2 x3 x3 x4 x6 x7 x8 x9 x9 x10 x10)
          (* x2 x2 x3 x3 x4 x4 x6 x6 x9 x10 x10)
          (* x1 x2 x2 x3 x3 x4 x5 x6 x7 x8 x8 x9 x9 x10)
          (* x1 x1 x2 x4 x4 x5 x5 x8 x8 x9 x9 x10)
          (* x3 x4 x4 x5 x6 x7 x7 x9 x10)))
    (define g10
      '(+ (* x1 x2 x2 x3 x3 x5 x5 x6 x6 x7 x8 x9 x9 x10 x10)
          (* x3 x8 x9 x9 x10 x10)
          (* x1 x2 x2 x3 x4 x5 x5 x6 x6 x8 x8 x9 x10)
          (* x1 x3 x6 x7 x8 x10)
          (* x4 x4 x5 x5 x6 x6 x7 x9 x9)))
    (check-true (gcd-test d10 f10 g10)))
   (test-case
    "10a"
    (define d10a
      '(+ (* 2 x1 x2 x2 x4 x4 x8 x9 x9 x10 x10)
          (* 3 x2 x2 x4 x5 x5 x6 x7 x9 x10 x10)
          (* 4 x1 x1 x2 x3 x5 x5 x7 x7 x9 x9)
          (* 5 x1 x3 x3 x4 x4 x7 x7 x9 x9)
          (* 6 x1 x1 x3 x4 x7 x7 x8 x8)
          7))
    (define f10a
      '(+ (* 8 x1 x2 x3 x3 x4 x6 x7 x8 x9 x9 x10 x10)
          (* 9 x2 x2 x3 x3 x4 x4 x6 x6 x9 x10 x10)
          (* 10 x1 x2 x2 x3 x3 x4 x5 x6 x7 x8 x8 x9 x9 x10)
          (* 11 x1 x1 x2 x4 x4 x5 x5 x8 x8 x9 x9 x10)
          (* 12 x3 x4 x4 x5 x6 x7 x7 x9 x10)
          13))
    (define g10a
      '(+ (* 14 x1 x2 x2 x3 x3 x5 x5 x6 x6 x7 x8 x9 x9 x10 x10)
          (* 15 x3 x8 x9 x9 x10 x10)
          (* 16 x1 x2 x2 x3 x4 x5 x5 x6 x6 x8 x8 x9 x10)
          (* 17 x1 x3 x6 x7 x8 x10)
          (* 18 x4 x4 x5 x5 x6 x6 x7 x9 x9)
          19))
    (check-true (gcd-test d10a f10a g10a)))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))