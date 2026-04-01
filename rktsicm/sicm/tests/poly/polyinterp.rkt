#lang racket/base

(require rackunit
         racket/list
         "../../poly/polyinterp.rkt"
         "../../simplify/pcf.rkt"
         "../helper.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "poly/polyinterp"
   ;; lagrange interpolating poly ... again (see also interp and interp-generic)
   (test-case
    "make-interp-poly" ;; numerical version
    (define xs '(1 2 3 4 5))
    (define ys '( 2 4 8 6 3))
    (define P (make-interp-poly ys xs))
    (for ([x (in-list xs)]
          [y (in-list ys)])
      (check-equal? (poly:value P x) y))
    (check-equal? (poly:value P 3/2) 223/128)
    (check-exn #px"MAKE-INTERP-POLY: abscissas not distinct" (λ () (make-interp-poly ys '(1 1 3 4 5)))))
   (test-case
    "random test"
    (define N (+ 2 (random 10)))
    (define xs (let lp ([prev 0][i 0])
                 (if (< i N)
                     (let ([nxt (+ prev 1 (random 5))]) (cons nxt (lp nxt (+ i 1))))
                     '())))
    (define ys (build-list N (λ (_) (- (random 800) 400))))
    (define P (make-interp-poly ys xs))
    (for ([x (in-list xs)]
          [y (in-list ys)])
      (check-equal? (poly:value P x) y)))
   ;; now via chebyshev
   (test-case
    "get-poly-and-errors"
    (check-within (get-poly-and-errors sin -4 4 4)
                  (list (poly:dense-> '(0 0.8174278749098369 0 -0.07027756789443923))
                        0.4712503502968348
                        0.09965935049235297)
                  1e-15)
    (check-within (get-poly-and-errors (λ (x) (poly:value (poly:dense-> '(1 5 3 9 8 4)) x)) 0 5 3)
                  (list (poly:dense-> '(929.7109375 -3189.921875 1267.375))
                        2061.5234375
                        928.7109375)
                  1e-10))
   (test-case
    "get-poly-and-erros - random"
    (define f (car (shuffle (list sin cos sqrt log))))
    (define a (+ .5 (random 10)))
    (define b (+ a 1 (random 10)))
    (define d (+ 2 (random 5)))
    (define Pee (get-poly-and-errors f a b d))
    (define P (car Pee))
    (define maxerr (cadr Pee))
    (check-true (<= (poly:degree P) d))
    (for ([i (in-range 10)])
      (define x (+ a (* (- b a) (random))))
      (check-true (<= (abs (- (f x) (poly:value P x))) maxerr))))
   (test-case
    "polynomial-function"
    (check-exn #px"Not a polynomial" (λ () ((polynomial-function #(1 2 3)) 4)))
    (for ([i (in-range 10)])
      (define P (poly:dense-> (build-list (+ 1 (random 10)) (λ (_) (- (random 800) 400)))))
      (define x (/ (- (random 800) 400) 100))
      (check-equal? ((polynomial-function P) x)
                    (poly:value P x))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))