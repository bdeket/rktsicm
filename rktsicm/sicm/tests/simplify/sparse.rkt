#lang racket/base

(require rackunit
         "../../simplify/sparse.rkt"
         "../../kernel-intr.rkt"
         "../helper.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "simplify/sparse"
   (check-equal? (map (λ (x) (cons (sparse-exponents x) (expression (sparse-coefficient x))))
                  (sparse-evaluate>
                   '(((2 3 0) . 3) ((1 1 1) . 1) ((0 0 1) . 4) ((0 0 0) . 1))
                   '(y z)))
                 '(((2) . (* 3 (expt y 3))) ((1) . (* y z)) ((0) . (+ 1 (* 4 z))))
                 #; ;equivalent?
                 '(((2) . (* 3 (expt y 3))) ((1) . (* y z)) ((0) . (* 4 z)) ((0) . 1)))
   (check-equal? (map (λ (x) (cons (sparse-exponents x) (expression (sparse-coefficient x))))
                  (sparse-evaluate<
                   '(((2 3 0) . 3) ((1 1 1) . 1) ((0 0 1) . 4) ((0 0 0) . 1))
                   '(x y)))
                 '(((1) . (+ 4 (* x y))) ((0) . (+ 1 (* 3 (expt x 2) (expt y 3))))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))