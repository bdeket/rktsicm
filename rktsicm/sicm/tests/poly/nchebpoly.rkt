#lang racket/base

(require rackunit
         racket/stream
         (only-in "../../kernel.rkt") ;; generics need to be loaded
         "../../poly/nchebpoly.rkt"
         "../../simplify/pcf.rkt"
         "../helper.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "poly/nchebpoly"
   (test-case
    "add-lists"
    (check-equal? (add-lists '() '()) '())
    (check-equal? (add-lists '(1 2 3) '()) '(1 2 3))
    (check-equal? (add-lists '() '(1 2 3)) '(1 2 3))
    (check-equal? (add-lists '(3 2) '(1 2 3)) '(4 4 3))
    (check-equal? (add-lists '(1 2 3) '(3 2)) '(4 4 3))
    )
   (test-case
    "scale-list"
    (check-equal? (scale-list 4 '()) '())
    (check-equal? (scale-list 4 '(1 2 3)) '(4 8 12)))
   (test-case
    "chebyshev-polynomials"
    (check-equal? (poly/termlist (chebyshev-polynomial 0)) '(1))
    (check-equal? (poly/termlist (chebyshev-polynomial 1)) '(1 0))
    (check-equal? (poly/termlist (chebyshev-polynomial 2)) '(2 0 -1))
    (check-equal? (poly/termlist (chebyshev-polynomial 3)) '(4 0 -3 0))
    (check-equal? (poly/termlist (chebyshev-polynomial 4)) '(8 0 -8 0 1)))
   (test-case
    "cheyshev-expansion"
    (check-equal? (stream-ref scaled-chebyshev-expansions 0)   '(1))
    (check-equal? (stream-ref scaled-chebyshev-expansions 1) '(0 1))
    (check-equal? (stream-ref scaled-chebyshev-expansions 2)   '(1 0 1))
    (check-equal? (stream-ref scaled-chebyshev-expansions 3) '(0 3 0 1))
    (check-equal? (stream-ref scaled-chebyshev-expansions 4)   '(3 0 4 0 1))

    (check-equal? (stream-ref chebyshev-expansions 0)   '(1))
    (check-equal? (stream-ref chebyshev-expansions 1) '(0 1))
    (check-equal? (stream-ref chebyshev-expansions 2)   '(1/2 0 1/2))
    (check-equal? (stream-ref chebyshev-expansions 3) '(0 3/4 0 1/4))
    (check-equal? (stream-ref chebyshev-expansions 4)   '(3/8 0 4/8 0 1/8)))
   (test-case
    "poly<->cheb-exp"
    (check-equal? (poly->cheb-exp (cheb-exp->poly '(1 2 3))) '(1 2 3))
    (check-equal? (poly->cheb-exp (cheb-exp->poly '(1))) '(1))
    (check-equal? (poly->cheb-exp (cheb-exp->poly '(4 0 8 0 9 0 2))) '(4 0 8 0 9 0 2))
    (check-equal? (poly->cheb-exp (poly:dense-> '(1 2 3 0 0))) '(5/2 2 3/2))
    (check-equal? (poly->cheb-exp (poly:dense-> '(1 2 3))) '(5/2 2 3/2))
    (check-exn #px"POLY->CHEB-EXP" (λ () (poly->cheb-exp (poly:dense-> '(+nan.0 8.058713286718672e-137))))))
   (test-case
    "trim-cheb-exp"
    (skip #;todo "make chebyshev-expansions *tagged-lists*")
    (check-equal? (trim-cheb-exp '(9 8 7 6 5 4 3 2 1 .5) 1) '(9 8 7 6 5 4 3 2 1))
    (check-equal? (trim-cheb-exp '(9 8 7 6 5 4 3 2 1 .5) 2) '(9 8 7 6 5 4 3 2))
    (check-equal? (trim-cheb-exp '(9 8 7 6 5 4 3 2 1 .5) 3) '(9 8 7 6 5 4 3 2))
    (check-equal? (trim-cheb-exp '(9 8 7 6 5 4 3 2 1 .5) 4) '(9 8 7 6 5 4 3))
    (check-equal? (trim-cheb-exp '(9) 4) '(9))
    (check-equal? (trim-cheb-exp '(9) 10) '(0)))
   (test-case
    "cheb-econ"
    (check-equal? (cheb-econ (poly:dense-> '(0 -1 1)) -1 3 1e-10) (poly:dense-> '(0 -1 1)))
    (check-equal? (cheb-econ (poly:dense-> '(0 -1 1)) -1 3 10) 0)
    (check-equal? (cheb-econ (poly:dense-> '(1 1 -1 2 -3 4 -5)) -2 2 1) (poly:dense-> '(1 1 -1 2 -3 4 -5)))
    (check-equal? (cheb-econ (poly:dense-> '(1 1 -1 2 -3 4 -5)) -2 2 100) (poly:dense-> '(57 -19 -88 22))))
   (test-case
    "root-list"
    (check-equal? (cheb-root-list 0) '())
    (check-equal? (cheb-root-list 1) '(0))
    (check-within (cheb-root-list 2) (list (/ (sqrt 2) -2) (/ (sqrt 2) 2)) 1e-15)
    (check-within (cheb-root-list 3) (list (- (sqrt 3/4)) 0 (sqrt 3/4)) 1e-15)
    (check-within (cheb-root-list 4) '(-0.9238795325112867 -0.38268343236508984 0.3826834323650897 0.9238795325112867) 1e-15))
   (test-case
    "first-n-cheb-values"
    (check-equal? (first-n-cheb-values 0 1) '())
    (check-equal? (first-n-cheb-values 0 1 'HALF) '())
    (check-equal? (first-n-cheb-values 1 1) '(1))
    (check-equal? (first-n-cheb-values 1 1 'HALF) '(1/2))
    (check-equal? (first-n-cheb-values 1 2) '(1))
    (check-equal? (first-n-cheb-values 1 2 'HALF) '(1/2))
    (check-equal? (first-n-cheb-values 2 1) '(1 1))
    (check-equal? (first-n-cheb-values 2 1 'HALF) '(1/2 1))
    (check-equal? (first-n-cheb-values 2 2) '(1 2))
    (check-equal? (first-n-cheb-values 2 2 'HALF) '(1/2 2))
    (check-equal? (first-n-cheb-values 3 1) '(1 1 1))
    (check-equal? (first-n-cheb-values 3 1 'HALF) '(1/2 1 1))
    (check-equal? (first-n-cheb-values 3 2) '(1 2 7))
    (check-equal? (first-n-cheb-values 3 2 'HALF) '(1/2 2 7))
    (check-equal? (first-n-cheb-values 4 2 'HALF) '(1/2 2 7 26)))
   (test-case
    "cheb-exp-value"
    (check-equal? (cheb-exp-value '(1) 1) 1)
    (check-equal? (cheb-exp-value '(1) 2) 1)
    (check-equal? (cheb-exp-value '(1 1) 2) 3)
    (check-equal? (cheb-exp-value '(1 1 1) 2) 10)
    (check-equal? (cheb-exp-value '(1 1 1 1) 2) 36)
    (check-equal? (cheb-exp-value '(4 3 2 1) 2) 50))
   (test-case
    "generate-cheb-exp"
    (define (step x) (if (< x 0) 0 1))
    (check-equal? (generate-cheb-exp values 0 1 1) '(1/2))
    (check-within (generate-cheb-exp values 0 1 2) '(1/2 1/2) 1e-15)
    (check-within (generate-cheb-exp values 0 1 3) '(1/2 1/2 0) 1e-15)
    (check-within (generate-cheb-exp step -1 1 4) '(1/2 0.6532814824381883 0 -0.27059805007309845) 1e-15)
    (check-exn #px"Bad interval" (λ () (generate-cheb-exp values 1 0 1))))
   (test-case
    "generate-approx-poly"
    (define (step x) (if (< x 0) 0 1))
    (check-equal? (generate-approx-poly values 0 1 1) (poly:dense-> '(1/2)))
    (check-within (generate-approx-poly values 0 1 2) (poly:dense-> '(0 1)) 1e-15)
    (check-within (generate-approx-poly values 0 1 3) (poly:dense-> '(0 1 1e-300)) 1.5e-15)
    (check-within (generate-approx-poly step -1 1 5) (poly:dense-> '(1 1.0514622242382679 -2 -0.5812340224042897 1.6)) 1e-15)
    (check-within (generate-approx-poly step -1 1 5 .2) (poly:dense-> '(8/10 1.0514622242382679 -4/10 -0.5812340224042897)) 1e-15))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))