#lang racket/base

(require rackunit
         "../../rkt/define.rkt")
(require/expose "../../rkt/define.rkt" [the-default-object])

(define the-tests
  (test-suite
   "rkt/define"
   (test-case
    "1"
    (define (do1 a b #:optional c)
      (if (default-object? c)
          (list 'do1 a b)
          (list 'do1 a b c)))
    (check-equal? (do1 1 2) '(do1 1 2))
    (check-equal? (do1 1 2 3) '(do1 1 2 3)))
  
   (test-case
    "2"
    (define (do2 a b)
      (list 'do2 a b))
    (check-equal? (do2 1 2) '(do2 1 2)))
  
   (test-case
    "3"
    (define do3 'do3)
    (define do3*)
    (check-true (symbol? do3*)))
  
   (test-case
    "4"
    (define (do4 a b #:optional c d)
      (list 'do4 a b c d))
    (check-equal? (do4 1 2) `(do4 1 2 ,the-default-object ,the-default-object))
    (check-equal? (do4 1 2 3) `(do4 1 2 3 ,the-default-object))
    (check-equal? (do4 1 2 3 4) '(do4 1 2 3 4)))
  
   (test-case
    "5"
    (define (do5 a b #:optional c d . e)
      (list 'do5 a b c d e))
    (check-equal? (do5 1 2 3 4 5 6 7) '(do5 1 2 3 4 (5 6 7)))
    (check-equal? (do5 1 2) `(do5 1 2 ,the-default-object ,the-default-object ())))
  
   (test-case
    "6"
    (define (do6 a b . c)
      (list 'do6 a b c))
    (check-equal? (do6 1 2 3 4 5 6) `(do6 1 2 (3 4 5 6))))

   (test-case
    "ab"
    (define (a #:optional b)
      (c b))
    (define (c #:optional b)
      (if (default-object? b)
          'ok
          'nok))
    (check-equal? (a) 'ok))

   (test-case
    "ab2"
    (define (a2 #:optional b) (c2 b))
    (define (c2 b) (if (default-object? b) 'ok 'nok))
    (check-equal? (a2) 'ok))

   (test-case
    "ab3"
    (define ((a3 #:optional b) c) (if (default-object? b) (list c) (list b c)))
    (check-equal? ((a3 1) 2) (list 1 2))
    (check-equal? ((a3) 2) (list 2)))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))