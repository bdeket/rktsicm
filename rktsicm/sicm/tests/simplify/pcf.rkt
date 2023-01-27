#lang racket/base

(require rackunit
         "../../simplify/pcf.rkt"
         "../../simplify/pcfpf/pcf-fpf.rkt")

(define the-tests
  (test-suite
   "simplify/pcf"
   (test-case
    "part 1"
    ;;; Test examples
    (define x-1 (poly/make-from-dense 1 '(1 -1)))
    (define x+1 (poly/make-from-dense 1 '(1 1)))
    (check-equal? (poly/gcd-euclid (poly/mul (poly/mul x-1 x-1) x+1)
                                   (poly/mul (poly/mul x+1 x+1) x-1))
                  '(*dense* 1 1 0 -1))
    (check-equal? (poly/gcd-euclid (poly/mul (poly/mul x+1 x+1) x-1)
                                   (poly/mul (poly/mul x-1 x-1) x+1))
                  '(*dense* 1 1 0 -1))
    
    (define k1 (poly/make-from-dense 1 '(1 0 1 0 -3 -3 8 2 -5)))
    (define k2 (poly/make-from-dense 1 '(3 0 5 0 -4 -9 21)))
    (check-equal? (poly/gcd-euclid k1 k2)
                  1)
    #;(check-equal? (poly/gcd-collins (poly/mul (poly/mul x-1 x-1) x+1)
                                    (poly/mul (poly/mul x+1 x+1) x-1))
                  '(*dense* 1 1 0 -1)))
   (test-case
    "part 2"
    (define p1 (poly/make-from-dense 1 '(1 1 1)))
    (define p2 (poly/make-from-dense 1 '(3 2)))
    (define p3 (poly/make-from-dense 1 '(5 3 2)))
    (define p4 (poly/mul (poly/expt p1 3) (poly/mul p2 p3)))
    (define p5 (poly/mul p1 (poly/expt p3 2)))
    (check-equal? (poly/gcd-euclid p4 p5)
                  (poly/make-from-dense 1 '(5 8 10 5 2)))
    (check-equal? (poly/mul p1 p3)
                  (poly/make-from-dense 1 '(5 8 10 5 2)))
    (define p6 (poly/mul (poly/expt p1 2) (poly/expt p3 2)))
    (check-equal? (poly/gcd-euclid p4 p6)
                  (poly/make-from-dense 1 '(5 13 23 23 17 7 2))))
   (test-case
    "derivative-partial"
    (let-values ([(P V) (pcf:expression-> '(+ (* x y z) (* x x z) (* y y)) values)])
      (check-equal? (pcf:->expression (poly/derivative-partial P 2) V)
                    '(+ (* z x) (* 2 y)))))
;;;(->expression
;;; (poly/derivative-partial
;;;  (->poly '(+ (* x y z) (* x x z) (* y y))
;;;	     '(x y z))
;;;  2)
;;; '(x y z))
;;;Value: (+ (* z x) (* 2 y))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))