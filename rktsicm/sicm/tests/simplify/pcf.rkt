#lang racket/base

(require rackunit
         "../../simplify/pcf.rkt"
         "../../simplify/pcfpf/pcf-fpf.rkt"
         "../../general/resource-limit.rkt"
         (only-in "../../kernel-intr.rkt" *machine-epsilon*)
         (submod "../helper.rkt" runner))

(define P0 (poly/make 2 (list (poly/make 1 '(2 1 0)) 1 0)))
(define P1 (pcf:expression-> '(+ (* x y z) (* x x z) (* 3 y y)) (λ (p v) p)))
(define bad-poly '(any 1 3))
(define S0 '(*sparse* 1))

(provide the-tests)
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
    (skip (check-equal? (poly/gcd-collins (poly/mul (poly/mul x-1 x-1) x+1)
                                          (poly/mul (poly/mul x+1 x+1) x-1))
                        '(*dense* 1 1 0 -1))))
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
                    '(+ (* z x) (* 2 y))))
    (skip "->expression and ->poly are not defined, but the above checks the same"
          ;;;(->expression
          ;;; (poly/derivative-partial
          ;;;  (->poly '(+ (* x y z) (* x x z) (* y y))
          ;;;	     '(x y z))
          ;;;  2)
          ;;; '(x y z))
          ;;;Value: (+ (* z x) (* 2 y))
          ))
   ;;**************************************************************************
   (test-case
    "pcf:expression->"
    (check-equal? (pcf:expression-> '(+ (+ (* x y z) (* x x z) (* y y)))
                                    (λ (p v) (pcf:->expression p v)))
                  '(+ (* (+ (* z x) (* z y)) x) (expt y 2)))
    (check-equal? (pcf:expression-> '(+ (+ (* x y z) (* x x z) (* y y)))
                                    (λ (p v) (pcf:->expression p v))
                                    (λ (a b) (not (string-ci<? (format "~a" a)
                                                               (format "~a" b)))))
                  '(+ (* (+ (* x y) (expt x 2)) z) (expt y 2))))
   ;;**************************************************************************
   (test-case
    "make"
    (check-equal? (poly/arity (poly/make-constant 1 1)) 0)
    (check-equal? (poly/arity (poly/make-constant 2 1)) 0)
    (check-equal? (poly/arity (poly/make-constant 2 (poly/make 1 '(1)))) 0)
    (check-equal? (poly/arity (poly/make-constant 2 (poly/make 1 '(0 1)))) 2)
    (check-equal? (pcf:->expression (poly/make-constant 3 (poly/make 1 '(1 1)))
                                    '(x y z))
                  '(+ 1 z))
    (check-exn #px"Bad constant -- POLY/MAKE-CONSTANT"
               (λ () (poly/make-constant 2 (poly/make 3 '(0 1)))))

    (check-equal? (pcf:->expression (poly/make-c*x^n 1 5 7) '(x))
                  '(* 5 (expt x 7)))
    (check-equal? (pcf:->expression (poly/make-c*x^n 3 (poly/make-c*x^n 2 3 4) 5) '(x y z))
                  '(* 3 (expt y 4) (expt x 5)))
    (check-equal? (pcf:->expression (poly/make-c*x^n 3 (poly/make-c*x^n 1 3 4) 5) '(x y z))
                  '(* 3 (expt z 4) (expt x 5)))
    (check-equal? (pcf:->expression (poly/make-identity 1) '(x)) 'x)
    (check-equal? (pcf:->expression (poly/make-identity 2) '(x y)) 'x))
   (test-case
    "identity?"
    (check-true (poly/identity? (poly/make-c*x^n 1 1 1)))
    (check-true (poly/identity? (poly/make 1 '(1 0))))
    (check-false (poly/identity? (poly/make-c*x^n 2 1 1)))
    (check-false (poly/identity? (poly/make 3 '(1 0))))
    (check-false (poly/identity? (poly/make-c*x^n 1 1 2)))
    (check-false (poly/identity? (poly/make 1 '(1))))
    (check-false (poly/identity? (poly/make 1 '(1 0 0))))
    (check-false (poly/identity? (poly/make 1 '(2 0)))))
   (test-case
    "monic?"
    (check-true (poly/monic? 1))
    (check-true (poly/monic? (poly/make 1 '(1 1))))
    (check-true (poly/monic? (poly/make-c*x^n 3 1 1)))
    (check-false (poly/monic? (poly/make 1 '(2 1))))
    (check-false (poly/monic? (poly/make-c*x^n 1 2 1))))
   (test-case
    "negative? zero? one?"
    (check-true (poly/zero? (poly/make 1 '(0))))
    (check-true (poly/zero? (poly/make-c*x^n 1 0 0)))
    (check-false (poly/zero? (poly/make 1 '(1 0))))
    (check-false (poly/zero? (poly/make-c*x^n 1 1 1)))
    (check-true (poly/one? (poly/make 1 '(1))))
    (check-true (poly/one? (poly/make-c*x^n 1 1 0)))
    (check-false (poly/one? (poly/make 1 '(2))))
    (check-false (poly/one? (poly/make-c*x^n 1 1 1)))
    (check-true (poly/negative? (poly/make 1 '(-1))))
    (check-true (poly/negative? (poly/make 1 '(-2 1))))
    (check-true (poly/negative? (poly/make-c*x^n 1 -3 1)))
    (check-false (poly/negative? (poly/make 1 '(3 -2 1)))))
   (test-case
    "equal?"
    (check-true (poly/equal? (poly/make 1 '(1))
                             (poly/make-c*x^n 1 1 0)))
    (check-true (poly/equal? (poly/make 2 '(1 0))
                             (poly/make-c*x^n 2 1 1)))
    (check-false (poly/equal? (poly/make 1 '(1))
                              (poly/make-c*x^n 1 1 1)))
    (check-false (poly/equal? (poly/make 1 '(1 1))
                              (poly/make-c*x^n 1 1 1)))
    (check-false (poly/equal? (poly/make 2 '(1 1))
                              (poly/make-c*x^n 2 1 1)))
    (check-false (poly/equal? (poly/make 2 '(1 1))
                              (poly/make-c*x^n 1 1 1)))
    (check-equal? (poly/check-same-arity? (poly/make 1 '(1 0))
                                          (poly/make-c*x^n 1 1 1))
                  1)
    (check-false (poly/check-same-arity? (poly/make 1 '(1 0))
                                         (poly/make-c*x^n 2 1 1)))
    (check-exn #px"Unequal arities -- POLY"
               (λ () (poly/check-same-arity (poly/make 1 '(1 0))
                                            (poly/make-c*x^n 2 1 1)))))
   (test-case
    "extend / contract"
    (check-equal? (let ([A (pcf:expression-> '(+ (* x y z) (* x y y)) (λ (p v) p))])
                    (pcf:->expression (poly/extend 1 (poly/extend 4 A))
                                      '(x n3 y z n1 n2)))
                  '(* (+ y z) y x))
    (check-true (poly/contractable? 0 (poly/make 1 '(1))))
    (check-false (poly/contractable? 0 (poly/make 1 '(1 0))))
    (check-true (poly/contractable? 1 (poly/make 2 '(1))))
    (let* ([A (pcf:expression-> '(+ (* x y z) (* x y y)) (λ (p v) p))]
           [B (poly/extend 1 (poly/extend 4 A))])
      (check-true (poly/contractable? 1 B))
      (check-true (poly/contractable? 4 B))
      (check-true (poly/contractable? 5 B))
      (check-false (poly/contractable? 1 A))
      (check-exn #px"Poly not contractable" (λ () (poly/contract 1 A)))
      (check-equal? (poly/contract 3 (poly/contract 4 (poly/contract 1 B)))
                    A)))
   (test-case
    "vars"
    (check-equal? (map (λ (x) (pcf:->expression x '(x y z))) (poly/make-vars 3))
                  '(x y z))
    (check-equal? (poly/make-vars 0) '()))
   (test-case
    "poly/add sub negate"
    (check-equal? (poly/add (poly/make 1 '(1)) (poly/make-c*x^n 1 2 0))
                  (poly/make-c*x^n 1 3 0))
    (check-equal? (poly/add (poly/make 1 '(3 2 1 0))
                            (poly/make 1 '(2 1 0)))
                  (poly/make 1 '(3 4 2 0)))
    (check-equal? (poly/add (poly/make 1 '(2 1 0))
                            (poly/make 1 '(3 2 1 0)))
                  (poly/make 1 '(3 4 2 0)))
    (check-equal? (poly/add (poly/make 1 '(3 2 1 0))
                            (poly/make 1 '(-3 -2 -1 0)))
                  0)
    (check-equal? (poly/add (poly/make 1 '(-3 -2 -1 0))
                            (poly/make 1 '(3 2 1 0)))
                  0)
    
    (check-equal? (poly/sub (poly/make-c*x^n 1 0 0) P0)
                  (poly/make 2 (list (poly/make 1 '(-2 -1 0)) -1 0)))
    (check-equal? (poly/sub (poly/make 1 '(1)) (poly/make-c*x^n 1 2 0))
                  (poly/make-c*x^n 1 -1 0))
    (check-equal? (poly/sub (poly/make 1 '(3 2 1 0))
                            (poly/make 1 '(2 1 0)))
                  (poly/make-c*x^n 1 3 3))
    (check-equal? (poly/sub (poly/make 1 '(2 1 0))
                            (poly/make 1 '(3 2 1 0)))
                  (poly/make-c*x^n 1 -3 3))
    (check-equal? (poly/sub (poly/make 1 '(3 2 1 0))
                            (poly/make 1 '(3 2 1 0)))
                  0)
    (check-equal? (poly/negate P0)
                  (poly/make 2 (list (poly/make 1 '(-2 -1 0)) -1 0))))
   (test-case
    "poly/scale - scale-1"
    (check-equal? (poly/scale P0 2)
                  (poly/make 2 (list (poly/make 1 '(4 2 0)) 2 0)))
    (check-equal? (poly/scale (poly/make 1 '(3)) 3)
                  (poly/make 1 '(9))))
   (test-case
    "poly/mul square"
    (check-equal? (poly/mul (poly/make 1 '(0)) (poly/make 1 '(1 0)))
                  poly/zero)
    (check-equal? (poly/mul (poly/make 1 '(1 0)) (poly/make 1 '(0)))
                  poly/zero)
    (check-equal? (poly/mul (poly/make 1 '(1)) P1)
                  P1)
    (check-equal? (poly/mul (poly/make 1 '(1 0)) (poly/make 1 '(1)))
                  (poly/make 1 '(1 0)))
    (check-equal? (poly/mul (poly/make 1 '(2)) (poly/make 1 '(3)))
                  (poly/make 1 '(6)))
    (check-equal? (pcf:->expression (poly/mul (poly/extend 0 P0) P1) '(x y z))
                  '(+ (* (+ (* (+ (* (+ 1 (* 2 z)) (expt z 2) y) z) y x)
                            (* (+ (* (+ 1 (* 2 z)) (expt z 2) y) z) (expt y 2)))
                         x)
                      (* (+ 3 (* (+ 3 (* 6 z)) z y)) (expt y 3))))
    (check-equal? (pcf:->expression (poly/mul (poly/extend 1 P0) P1) '(x y z))
                  '(* (+ (* (+ (* (+ (* (+ 1 (* 2 z)) (expt z 2) x)
                                     (* (+ 1 (* 2 z)) (expt z 2) y) z) x)
                               (* (+ (* (+ 3 (* 6 z)) z y) z) y)) x)
                         (* 3 (expt y 2)))
                      x))
    (check-equal? (poly/square P0) (poly/mul P0 P0)))
   (test-case
    "poly/expt"
    (check-equal? (poly/expt P0 2) (poly/square P0))
    (check-equal? (poly/expt P1 4) (poly/square (poly/square P1)))
    (check-equal? (poly/expt poly/one 8) poly/one)
    (check-equal? (poly/expt poly/zero 6) poly/zero)
    (check-equal? (poly/expt P0 0) poly/one)
    (check-exn #px"Can only raise a PCF to an exact integer power" (λ () (poly/expt P0 1.5)))
    (check-exn #px"No inverse \\(POLY/EXPT\\):" (λ () (poly/expt P0 -1)))
    (skip ;; !!! unreachable and '= 1'
     (check-exn #px"0^0 -- POLY/EXPT" (λ () (poly/expt poly/zero 0)))))
   (test-case
    "poly/div quotient - divisible?"
    (define p1 (pcf:expression-> '(+ (* s s s) (* 3 s s) (* 4 s) 5) (λ (p v) p)))
    (define p2 (pcf:expression-> '(+ x 1) (λ (p v) p)))
    (define p3 (pcf:expression-> '(+ (* s s) (* 2 s) 2) (λ (p v) p)))
    (define p4 (poly/make 1 '(3)))
    (define p5 (poly/make 1 '(3 0)))
    (check-equal? (poly/div p1 p2 list) (list p3 p4))
    (check-exn #px"Divide by zero \\(POLY/DIV\\):" (λ () (poly/div P0 poly/zero values)))
    (check-equal? (poly/div 5 2 list) '(5/2 0))
    (check-equal? (poly/div P0 1 list) (list P0 0))
    (check-equal? (poly/div poly/zero P0 list) (list poly/zero 0))
    (check-false (poly/not-divisible? (poly/add p1 p5) p2))
    (check-equal? (poly/quotient (poly/add p1 p5) p2) (poly/add p3 3))
    (check-true (poly/not-divisible? p1 p2))
    (check-exn #px"Inexact division \\(POLY/QUOTIENT\\):" (λ () (poly/quotient p1 p2))))
   (test-case
    "poly/map-terms - normalize"
    (check-equal? (map-poly-terms add1 '(*dense* 1 3 2 1))
                  '(*dense* 1 4 3 2))
    (define p1 (poly/make 2 '(1 2 1)))
    (define p2 (poly/make-c*x^n 2 (poly/make-c*x^n 1 1 1) 0))
    (check-equal? (pcf:->expression (map-poly-terms (λ (x) (poly/make-c*x^n 1 x 1)) p1) '(x y))
                  (pcf:->expression (poly/mul p1 p2) '(x y)))
    (check-equal? (poly/normalize p1 2) (poly/mul p1 1/2))
    (check-equal? (poly/normalize (poly/mul p1 p2) (poly/make-c*x^n 1 1 1)) p1)
    (check-equal? (poly/normalize P0 poly/one) P0)
    (check-exn #px"Divide by zero \\(POLY/NORMALIZE\\):" (λ () (poly/normalize P0 poly/zero))))
   (test-case
    "poly/gcd/euclid gcd-euclid"
    (check-equal? (poly/gcd-euclid P1 (poly/extend 2 poly/identity)) 1)
    (check-exn #px"Divide by zero \\(POLY/PSEUDO-REMAINDER\\):"
               (λ () (poly/pseudo-remainder P0 poly/zero values)))
    (check-equal? ((poly/content-maker (λ (a b c d) (error "not called")))
                   poly/zero (λ _ 'win) (λ _ 'lose)) 'win)
    (check-false (with-limited-time 0
                                    (λ ()
                                      (poly/gcd-euclid
                                       (poly/make 3 '(5 2 9 4 8 6 7 5))
                                       P1))))
    (check-equal? (poly/gcd-euclid (poly/mul (poly/mul P1 P1)
                                             (poly/extend 0 P0))
                                   P1)
                  P1)
    (check-equal? (poly/gcd-euclid poly/zero P0) P0)
    (check-equal? (poly/gcd-euclid P0 poly/zero) P0)
    (check-equal? (poly/gcd-euclid 3 P0) 1))
   (test-case
    "poly/derivative-partial partial-derivative derivative-principal"
    (check-equal? (poly/partial-derivative P0 '(0))
                  (poly/make 2 (list (poly/make 1 '(4 2 0)) 1)))
    (check-equal? (poly/partial-derivative P0 '(1))
                  (poly/make-c*x^n 2 (poly/make 1 '(4 1)) 2))
    (check-exn #px"Bad varnum -- POLY/DERIVATIVE-PARTIAL" (λ () (poly/partial-derivative P0 '(2))))
    (check-equal? (poly/partial-derivative 1 '(1)) 0)
    (check-equal? (poly/partial-derivative P0 '(-1)) 0)
    (check-equal? (poly/derivative-principal 1) 0))
   (test-case
    "poly/")
   (test-case
    "poly/horner-univariate - horner - helper - hh - with-error"
    (check-equal? (poly/horner-univariate 1 1) 1)
    (check-equal? (poly/horner-univariate 2 1) 2)
    (check-equal? (poly/horner-univariate (poly/make 1 '(1 1)) 1) 2)
    (check-equal? (poly/horner-univariate (poly/make 1 '(1 1)) 2) 3)
    (check-equal? (poly/horner-univariate (poly/make 1 '(2 2)) 2) 6)
    
    (check-equal? (poly/horner 1 '(1 2 3)) 1)
    (check-equal? (poly/horner 2 '(1 2 3)) 2)
    (check-equal? (poly/horner P0 '(0 0)) 0)
    (check-equal? (poly/horner P0 '(1 1)) 4)
    (check-equal? (poly/horner P1 '(1 1 1)) 5)
    (check-exn #px"Wrong number of args -- POLY/HORNER" (λ () (poly/horner P0 '(1))))
    
    (check-equal? (poly/horner-with-error 5 1 vector) #(5 0 0 0))
    (check-equal? (poly/horner-with-error (poly/make 1 '(1 0)) 4 vector)
                  (vector 4 1 0 (* 2 4 *machine-epsilon*)))
    (check-exn #px"Wrong arity poly -- POLY/HORNER-WITH-ERROR"
               (λ () (poly/horner-with-error P0 1+i vector)))
    (check-within (poly/horner-with-error (poly/make 1 '(1 0 0 0 1 0 1 0)) 1+i vector)
                  #(7-5i 1-50i -162-162i 1e-14) 1e-13)
    (check-equal? (poly:value P0 1 1) 4)
    (check-equal? (poly:principal-value 3 3) 3))
   (test-case
    "poly/arg-scale arg-shift"
    (check-equal? (poly/arg-scale (poly/make-c*x^n 1 2 3) '(4))
                  (poly/make-c*x^n 1 128 3))
    (check-equal? (poly/arg-scale P0 '(1 0)) (poly/make 2 '(1 0)))
    (check-equal? (poly/arg-scale P0 '(0 1)) 0)
    (check-equal? (poly/arg-scale P1 '(1 0 0)) 0)
    (check-equal? (poly/arg-scale P1 '(0 1 0)) (poly/make 3 (list (poly/make-c*x^n 2 3 2))))
    (check-equal? (poly/arg-scale P1 '(0 0 1)) 0)

    (check-equal? (poly/arg-shift (poly/make-c*x^n 1 2 3) '(4))
                  (poly/make 1 '(2 24 96 128)))
    (check-equal? (poly/arg-shift P0 '(1 0))
                  (poly/make 2 (list (poly/make 1 '(2 1 0))
                                     (poly/make 1 '(4 2 1))
                                     (poly/make 1 '(2 1 1)))))
    (check-equal? (poly/arg-shift P0 '(0 1))
                  (poly/make 2 (list (poly/make 1 '(2 5 3)) 1 0)))
    (check-equal? (poly/arg-shift P1 '(1 0 0))
                  (poly/make 3 (list (poly/make 2 (list (poly/make 1 '(1 0))))
                                     (poly/make 2 (list (poly/make 1 '(1 0)) (poly/make 1 '(2 0))))
                                     (poly/make 2 (list 3 (poly/make 1 '(1 0)) (poly/make 1 '(1 0)))))))
    (check-equal? (poly/arg-shift P1 '(0 1 0))
                  (poly/make 3 (list (poly/make 2 (list (poly/make 1 '(1 0))))
                                     (poly/make 2 (list (poly/make 1 '(1 0)) (poly/make 1 '(1 0))))
                                     (poly/make 2 (list 3 6 3)))))
    (check-equal? (poly/arg-shift P1 '(0 0 1))
                  (poly/make 3 (list (poly/make 2 (list (poly/make 1 '(1 1))))
                                     (poly/make 2 (list (poly/make 1 '(1 1)) 0))
                                     (poly/make-c*x^n 2 3 2)))))
   (test-case
    "poly/abs"
    (check-equal? (poly/abs -4) 4)
    (check-equal? (poly/abs P0) P0)
    (check-equal? (poly/abs (poly/negate P0)) P0))
   (test-case
    "poly/leading-base-coefficient"
    (check-equal? (poly/leading-base-coefficient 3) 3)
    (check-equal? (poly/leading-base-coefficient (poly/make 1 '(3 2 1))) 3)
    (check-equal? (poly/leading-base-coefficient (poly/make 2 (list (poly/make 2 '(3 2 1)) 2 1))) 3))
   (test-case
    "poly/pcf?"
    (check-true (pcf? 3))
    (check-true (pcf? (poly/make 2 '(12 2))))
    (check-true (pcf? (poly/make-c*x^n 5 4 3)))
    (check-false (pcf? '(1 2 3)))
    
    (check-true (explicit-pcf? (poly/make 2 '(12 2))))
    (check-true (explicit-pcf? (poly/make-c*x^n 5 4 3)))
    (check-false (explicit-pcf? 3))
    (check-false (explicit-pcf? '(1 2 3)))
    
    (check-equal? (poly/type 3) '*dense*)
    (check-equal? (poly/type (poly/make 2 '(12 2))) '*dense*)
    (check-equal? (poly/type (poly/make-c*x^n 5 4 3)) '*sparse*)

    (check-equal? (poly/arity 3) 0)
    (check-equal? (poly/arity (poly/make 2 '(12 2))) 2)
    (check-equal? (poly/arity (poly/make-c*x^n 5 4 3)) 5)

    (check-equal? (poly/termlist 0) '())
    (check-equal? (poly/termlist 3) '(3))
    (check-equal? (poly/termlist (poly/make 2 '(12 2))) '(12 2))
    (check-equal? (poly/termlist (poly/make-c*x^n 5 4 3)) '((3 . 4)))
    (check-equal? (poly/termlist P0) (list (poly/make 1 '(2 1 0)) 1 0))

    (check-true (poly/sparse? (poly/make-c*x^n 5 4 3)))
    (check-false (poly/sparse? (poly/make-from-dense 2 '(12 2))))
    (check-false (poly/sparse? 3))

    (check-equal? (poly/make-from-sparse 5 '((3 . 4))) (poly/make-c*x^n 5 4 3))
    (check-equal? (poly/make-from-sparse 5 '((3 . 4)(2 . 1)))
                  (poly/add (poly/make-c*x^n 5 4 3) (poly/make-c*x^n 5 1 2)))
    
    (check-false (poly/dense? (poly/make-c*x^n 5 4 3)))
    (check-true (poly/dense? (poly/make-from-dense 2 '(12 2))))
    (check-true (poly/dense? 3))

    (check-equal? (poly/degree (poly/make-c*x^n 5 4 3)) 3)
    (check-equal? (poly/degree (poly/make 3 '(5 4 3 2 1 0))) 5)
    (check-equal? (poly/degree 5) 0)
    (check-equal? (poly/degree 0) -1)
    (check-equal? (poly/degree S0) -1)
    (check-exn #px"Bad type -- POLY/DEGREE" (λ () (poly/degree bad-poly)))

    (check-equal? (poly/leading-coefficient 3) 3)
    (check-equal? (poly/leading-coefficient 0) 0)
    (check-equal? (poly/leading-coefficient (poly/make-from-sparse 5 '((3 . 4)(2 . 1)))) 4)
    (check-equal? (poly/leading-coefficient (poly/make 1 '(6 5 2))) 6)
    (check-equal? (poly/leading-coefficient S0) 0)
    (check-exn #px"Bad type -- POLY/LEADING-COEFFICIENT" (λ () (poly/leading-coefficient bad-poly)))

    (check-equal? (poly/except-leading-term 0 3) 0)
    (skip ;; !!!!
     (check-equal? (poly/except-leading-term 0 0) 0))
    (check-equal? (poly/except-leading-term 5 (poly/make-from-sparse 5 '((3 . 4)(2 . 1))))
                  (poly/make-from-sparse 5 '((2 . 1))))
    (check-equal? (poly/except-leading-term 1 (poly/make 1 '(6 5 2))) (poly/make 1 '(5 2)))
    (check-exn #px"Bad type -- POLY/EXCEPT-LEADING-TERM" (λ () (poly/except-leading-term 9 bad-poly))))
   (test-case
    "poly/adjoin"
    (check-exn #px"Bad type -- POLY/ADJOIN" (λ () (poly/adjoin 9 110 300 bad-poly)))
    (check-equal? (poly/adjoin 3 6 1 (poly/make 3 '(5 4 3 2 1 0))) (poly/make 3 '(1 5 4 3 2 1 0)))
    (check-equal? (poly/adjoin 3 7 1 (poly/make 3 '(5 4 3 2 1 0))) (poly/make 3 '(1 0 5 4 3 2 1 0)))
    (check-equal? (poly/adjoin 3 8 1 (poly/make 3 '(5 4 3 2 1 0)))
                  (poly/make-from-sparse 3 '((8 . 1) (5 . 5) (4 . 4) (3 . 3) (2 . 2) (1 . 1))))
    (check-equal? (poly/dense/adjoin 8 1 '(5 4 3 2 1 0)) '(1 0 0 5 4 3 2 1 0))
    (check-exn #px"Term not in order \\(POLY/ADJOIN\\):"
               (λ () (poly/adjoin 3 3 1 (poly/make 3 '(5 4 3 2 1 0)))))
    (skip "arity not correct!" ;!!
     (check-equal? (poly/adjoin 0 6 1 (poly/make 3 (list (poly/make 2 '(1 1)) 4 3 2 1 0)))
                   (poly/make 0 (list 1 (poly/make 2 '(1 1)) 4 3 2 1 0)))))
   (test-case
    "poly/coefficient coefficients"
    (check-equal? (poly/coefficient (poly/make 3 '(5 4 3 2 1 0)) 3) 3)
    (check-equal? (poly/coefficient (poly/make-from-sparse 1 '((5 . 5)(3 . 3)(1 . 1))) 3) 3)
    (check-equal? (poly/coefficient (poly/make-from-sparse 1 '((5 . 5)(3 . 3)(1 . 1))) 2) 0)
    (check-equal? (poly/coefficient S0 3) 0)
    (check-exn #px"POLY/DENSE/COEFFICIENT: contract violation\n  expected: nonnegative fixnum\n"
               (λ () (poly/coefficient (poly/make 3 '(5 4 3 2 1 0)) -2)))
    (check-exn #px"POLY/SPARSE/COEFFICIENT: contract violation\n  expected: nonnegative fixnum\n"
               (λ () (poly/coefficient (poly/make-from-sparse 1 '((5 . 5)(3 . 3)(1 . 1))) -2)))
    (check-exn #px"Bad type -- POLY/COEFFICIENT" (λ () (poly/coefficient bad-poly 9)))

    (check-equal? (poly/coefficients (poly/make 3 '(5 4 3 2 1 0))) '(5 4 3 2 1))
    (check-equal? (poly/coefficients (poly/make-from-sparse 1 '((5 . 5)(3 . 3)(1 . 1)))) '(5 3 1))
    (check-exn #px"Bad type -- POLY/COEFFICIENTS" (λ () (poly/coefficients bad-poly)))

    (check-equal? (poly/base-coefficients (poly/make 3 '(5 4 3 2 1 0))) '(1 2 3 4 5))
    (check-equal? (poly/base-coefficients (poly/make 2 (list (poly/make 1 '(30 31 32))
                                                             (poly/make 1 '(20 21))
                                                             (poly/make 1 '(10))
                                                             1)))
                  '(1 10 20 21 30 31 32)))
   (test-case
    "poly/principal-reverse"
    (check-equal? (poly/principal-reverse (poly/make 1 '(5 4 3 2 1 0)))
                  (poly/make 1 '(1 2 3 4 5)))
    (check-equal? (poly/principal-reverse (poly/make-from-sparse 1 '((5 . 5)(3 . 3)(1 . 1))))
                  (poly/make-from-sparse 1 '((4 . 1)(2 . 3)(0 . 5))))
    (check-equal? (poly/principal-reverse S0) 0)
    (check-exn #px"Bad type -- POLY/PRINCIPAL-REVERSE" (λ () (poly/principal-reverse bad-poly))))
   (test-case
    "poly/->dense->sparse->"
    (check-equal? (poly/->dense P0) P0)
    (check-equal? (poly/->dense (poly/->sparse P0)) P0)
    (check-equal? (poly/->sparse (poly/make 1 '(1 0))) (poly/make-from-sparse 1 '((1 . 1))))
    (check-equal? (poly/->dense (poly/make-from-sparse 1 '((1 . 1)))) (poly/make 1 '(1 0)))
    (check-equal? (poly/->sparse (poly/make-from-sparse 1 '((1 . 1))))
                  (poly/make-from-sparse 1 '((1 . 1))))
    (check-equal? (poly/->dense S0) 0)
    (check-equal? (poly/->dense (poly/make-c*x^n 2 3 4)) (poly/make 2 '(3 0 0 0 0)))
    (check-equal? (poly/->dense (poly/add (poly/make-c*x^n 2 3 4) 1)) (poly/make 2 '(3 0 0 0 1)))
    (check-exn #px"Bad type -- POLY/->DENSE" (λ () (poly/->dense bad-poly)))
    (check-exn #px"Bad type -- POLY/->SPARSE" (λ () (poly/->sparse bad-poly))))
   (test-case
    "poly/lowest-order trailing-coefficient"
    (check-equal? (poly/lowest-order 0) 0)
    (check-equal? (poly/lowest-order 3) 0)
    (check-equal? (poly/lowest-order P0) 1)
    (check-equal? (poly/lowest-order P1) 0)
    (check-equal? (poly/lowest-order (poly/make-c*x^n 5 4 3)) 3)
    (check-exn #px"Bad type -- POLY/LOWEST-ORDER" (λ () (poly/lowest-order bad-poly)))
    (check-equal? (poly/trailing-coefficient 0) 0)
    (check-equal? (poly/trailing-coefficient 3) 3)
    (check-equal? (poly/trailing-coefficient P0) 1)
    (check-equal? (poly/trailing-coefficient P1) (poly/make-c*x^n 2 3 2))
    (check-equal? (poly/trailing-coefficient (poly/make-c*x^n 5 4 3)) 4)
    (check-equal? (poly/trailing-coefficient (poly/add 3 (poly/make-c*x^n 5 4 3))) 3)
    (check-exn #px"Bad type -- POLY/TRAILING-COEFFICIENT" (λ () (poly/trailing-coefficient bad-poly))))
   (test-case
    "poly:dense-> ->dense ->expression ->lambda"
    (check-equal? (poly:dense-> '(1 2 3 4)) (poly/make 1 '(4 3 2 1)))
    (check-equal? (poly:->dense (poly/make 1 '(4 3 2 1))) '(1 2 3 4))
    (check-equal? (poly:->dense 5) '(5))
    (check-equal? (pcf:->expression 3 '()) 3)
    (check-equal? (pcf:->expression 3 '(x)) 3)
    (check-equal? (pcf:->expression (poly/make 1 '(4 3 2 1)) '(x))
                  '(+ 1 (* (+ 2 (* (+ 3 (* 4 x)) x)) x)))
    (check-equal? (pcf:->expression P1 '(x y z))
                  '(+ (* (+ (* z x) (* z y)) x) (* 3 (expt y 2))))
    (check-exn #px"Poly arity not = vars supplied -- PCF:->EXPRESSION"
               (λ () (pcf:->expression P1 '(x y))))
    (check-equal? (poly:->lambda P1)
                  '(lambda (x.0 x.1 x.2)
                     (+ (* (+ (* x.2 x.0) (* x.2 x.1)) x.0) (* 3 (expt x.1 2))))))
   (test-case
    "+$poly -$poly *$poly"
    (check-equal? (+$poly) 0)
    (check-equal? (+$poly P0 P0 P0) (poly/scale P0 3))
    (check-equal? (-$poly) 0)
    (check-equal? (-$poly P0 P0 P0) (poly/scale P0 -1))
    (check-equal? (*$poly) 1)
    (check-equal? (*$poly P0 P0 P0) (poly/expt P0 3)))
   
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))