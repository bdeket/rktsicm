#lang s-exp "../../kernel.rkt"

(require rackunit
         "../../rkt/glue.rkt"
         "../../general/assert.rkt"
         "../../simplify.rkt"
         "../helper.rkt")

(define (gcd-test d f g)
  (let ((pd (pcf:expression-> d (lambda (p v) p)))
	(pf (pcf:expression-> f (lambda (p v) p)))
	(pg (pcf:expression-> g (lambda (p v) p))))
    (poly:= (poly:gcd (poly:* pd pf) (poly:* pd pg))
	    pd)))

(define the-tests
  (test-suite
   "simplify/rz-test-cases"
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
    "4a"
    (define d4a
      '(+ (* x1 x1 x1 x2 x2 x3 x3 x4 1 1)
          (* x1 x2 x2 1 1)
          (* x1 x1 x1 x3 x4 x4 1)
          (* x1 x1 x1 x2 x3 x3 x4 1)
          (* x1 x1 x2 x3 x3 x4 x4)))
    (define f4a
      '(+ (* x1 x2 x2 1 1)
          (* x1 x2 x3 x3 x4 1)
          (* x1 x2 x3 x3 x4 x4)
          (* x1 x2 x2 x4 x4)
          1))
    (define g4a
      '(+ (* x1 x3 x3 x4 1 1)
          (* x2 1 1)
          (* x1 x2 x4 1)
          (* x2 1)
          (* x1 x2 x3 x4 x4)))
    (check-true (gcd-test d4a f4a g4a)))
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
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))