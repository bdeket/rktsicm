#lang racket/base

(require rackunit
         "../../kernel-intr.rkt"
         "../../kernel/numeric.rkt"
         "../../rkt/fixnum.rkt"
         "../helper.rkt"
         )

(provide the-tests)
(define the-tests
  (test-suite
   "kernel/numeric"
   (test-case
    "ORIG:stirling-first-kind"
    (check-equal? (stirling-first-kind 1 1) 1)
    (check-equal? (stirling-first-kind 2 1) 1)
    (check-equal? (stirling-first-kind 2 2) 1)
    (check-equal? (stirling-first-kind 3 1) 2)
    (check-equal? (stirling-first-kind 3 2) 3)
    (check-equal? (stirling-first-kind 5 2) 50)
    (check-equal? (stirling-first-kind 7 3) 1624)
    )

   (test-case
    "ORIG:stirling-second-kind"
    (check-equal? (stirling-second-kind 5 3) 25)
    )

   (test-case
    "ORIG:sigma"
    (check-equal? (g:sigma (lambda (x) (/ 1.0 x)) 1 10000000)
                  16.695311365857272)
    (check-equal? (sigma (lambda (x) (/ 1.0 x)) 1 10000000)
                  16.69531136585985))
   (test-case
    "harmonic - slow"
    (define (Hn n)
      (/ (stirling-first-kind (+ n 1) 2)
         (factorial n)))
    (check-simplified? (exact->inexact (Hn 300))
                       6.282663880299504
                       #:timeout 3))
   (test-case
    "harmonic"
    (check-equal? (let lp ((i 1) (sum 0.0))
                    (if (> i 300)
                        sum
                        (lp (+ i 1) (+ sum (/ 1.0 i)))))
                  6.282663880299502)

    (check-equal? (let lp ((i 300) (sum 0.0))
                    (if (= i 0)
                        sum
                        (lp (- i 1) (+ sum (/ 1.0 i)))))
                  6.282663880299501)

    (check-equal? (g:sigma (lambda (x) (/ 1.0 x)) 1 300)
                  6.282663880299502))
   (test-case
    "geometric"
    (define write-line values)
    (define (geometric a r n)
      (define (sigma-KahanBabushkaNeumaier f low high)
        (let lp ((i low) (sum 0) (c 0))
          (if (fix:> i high)
              sum
              (let* ((y (- (f i) c)) (t (+ sum y)))
                (lp (fix:+ i 1) t (- (- t sum) y))))))
      (list (let lp> ((k 0) (sum 0.0))
              (if (> k n)
                  (write-line `(forward-order ,sum))
                  (lp> (+ k 1) (+ sum (* a (expt r k))))))
            (write-line
             `(g:sigma
               ,(g:sigma				;generic sigma is naive
                 (lambda (k)
                   (exact->inexact (* a (expt r k))))
                 0 n)))
            (let lp< ((k n) (sum 0.0))
              (if (< k 0)
                  (write-line `(reverse-order ,sum))
                  (lp< (- k 1) (+ sum (* a (expt r k))))))
            (write-line
             `(kahan-method
               ,(sigma-KahanBabushkaNeumaier
                 (lambda (k)
                   (exact->inexact (* a (expt r k))))
                 0 n)))  
            (write-line
             `(explicit-formula
               ,(exact->inexact (/ (* a (- 1 (expt r (+ n 1))))
                                   (- 1 r)))))))

    (check-equal? (geometric 1 0.5001 200)
                  '((forward-order 2.0004000800160022)
                    (g:sigma 2.0004000800160022)
                    (reverse-order 2.000400080016003)
                    (kahan-method 2.000400080016003)
                    (explicit-formula 2.000400080016003))))
   ;; ************************************************************************************
   (test-case
    "rkt-extra"
    (check-true (exact-rational? 2/4))
    (check-true (exact-rational? 9))
    (check-false (exact-rational? 9.))
    (check-false (exact-rational? +inf.0))
    (check-false (exact-rational? #i1/2))
    (check-true  (exact-complex? 1+4i))
    (check-false (exact-complex? 1+4.i))
    (check-false (exact-complex? 1.+4i))
    (check-false (exact-complex? 1.+4.i))
    (define ID5/3 (integer-divide 5 3))
    (check-equal? (integer-divide-quotient ID5/3) 1)
    (check-equal? (integer-divide-remainder ID5/3) 2)
    (define ID5/3* (integer-divide 5. 3.))
    (check-equal? (integer-divide-quotient ID5/3*) 1)
    (check-equal? (integer-divide-remainder ID5/3*) 2)
    (check-equal? (complex-denominator 4+1/5i) 5)
    (check-equal? (complex-denominator 4/3+1/5i) 15))
   (test-case
    "definitions"
    (check-equal? (+ zero one -one two three
                     pi -pi pi/6 -pi/6 pi/4 -pi/4 pi/3 -pi/3 pi/2 -pi/2 2pi -2pi
                     :zero :one :-one :two :three
                     :pi :+pi :-pi :pi/6 :+pi/6 :-pi/6 :pi/4 :+pi/4 :-pi/4 :pi/3 :+pi/3 :-pi/3 :pi/2 :+pi/2 :-pi/2 :2pi :+2pi :-2pi
                     *machine-epsilon* *sqrt-machine-epsilon*
                     :euler :phi :ln2 :ln10 :minlog)
                  -971.4572492801368)
    (check-equal? (ulp 1.) *machine-epsilon*)
    (check-equal? (ulpr 1.) (/ *machine-epsilon* 2))
    (check-true (exact-zero? 0))
    (check-false (exact-zero? 0.))
    (check-true (exact-one? 1))
    (check-false (exact-one? 1.))
    (check-true (one? 1))
    (check-true (one? 1.))
    (check-false (one? (+ 1 *machine-epsilon*))))
   (test-case
    "sgn"
    (check-equal? (sgn -4.8) -1)
    (check-equal? (sgn -inf.0) -1)
    (check-equal? (sgn +inf.0) 1)
    (check-equal? (sgn -nan.0) 1)
    (check-equal? (sgn -0.0) 1)
    (check-equal? (sgn 4.2-3i) 1)
    (check-equal? (sgn -4.2+3i) -1))
   (test-case
    "exp/log : 2/10"
    (check-equal? (log10 10) 1.)
    (check-equal? (log10 100) 2.)
    (check-equal? (log2 2) 1.)
    (check-equal? (log2 4) 2.)
    (check-equal? (exp2 1) 2)
    (check-equal? (exp2 2) 4)
    (check-equal? (exp10 1) 10)
    (check-equal? (exp10 2) 100)
    (check-equal? (safelog 100) (log 100))
    (check-equal? (safelog 4.9e-324) (log 4.9e-324))
    (check-exn #px"Out of range -- SAFELOG" (λ () (safelog 0.))))
   (test-case
    "modpi (principal-value)"
    (check-equal? ((principal-value :pi) 1) 1)
    (check-equal? ((principal-value :pi/6) 1) (- 1 :2pi))
    (check-equal? ((principal-value :pi) -4) (+ -4 :2pi))
    (check-equal? (principal-value-minus-pi-to-pi 1) 1)
    (check-equal? (principal-value-minus-pi-to-pi 4) (- 4 :2pi))
    (check-equal? (principal-value-minus-pi-to-pi -4) (+ -4 :2pi))
    (check-equal? (principal-value-minus-pi-to-pi -4) (+ -4 :2pi))
    (check-equal? (principal-value-zero-to-2pi 1) 1)
    (check-equal? (principal-value-zero-to-2pi -1) (+ -1 :2pi))
    (check-equal? (principal-value-zero-to-2pi 8) (- 8 :2pi))
    (check-equal? ((principal-range 2) 9/10) 9/10)
    (check-equal? ((principal-range 2) -11/10) 9/10)
    (check-equal? ((principal-range 2) 11/10) -9/10))
   (test-case
    "base-op"
    (local-require math/flonum)
    (define (tst f1 f2 [R (* (random) 800)])
      (check-equal? (f1 R) (f2 R) (format "~a(~a)" f1 R)))
    (tst square (λ (R) (expt R 2)))
    (tst cube (λ (R) (expt R 3)))
    (tst negate -)
    (tst invert /)
    (tst cot (λ (R) (fl/ (fltan R))))
    (tst sec (λ (R) (fl/ (flcos R))))
    (tst csc (λ (R) (fl/ (flsin R))))
    (tst sinh flsinh)
    (tst cosh flcosh)
    (tst tanh fltanh (* (random) 500))
    (skip ;; !!! beter start using math/flonum in numeric... ?
          ;; or (/ (- 1 (exp -2 x)) (+ 1 (exp -2 x))) if x>1
     (tst tanh fltanh 766.))
    (tst sech (λ (R) (fl/ (flcosh R))))
    (tst csch (λ (R) (fl/ (flsinh R)))))
   (test-case
    "=~"
    (check-true  (~0? 0))
    (check-true  (~0? 1e-15))
    (check-false (~0? 1e-10))
    (check-true  (~0? 1e-120 1e-100))
    (check-false (~0? 1e-15 1e-100))
    (check-false (close-enuf? 1 2 1e-15))
    (check-true (close-enuf? 1 2 2))
    (check-true (close-enuf? 1+1/10i 11/10-1/20i 2/10)))
   (test-case
    "gcd"
    (check-equal? (make-rational 3 7) 3/7)
    (check-exn #px"assertion failed" (λ () (make-rational 3. 5.)))
    (check-equal? (gcd-rational 5/8 3/7) 1)
    (check-equal? (gcd-rational 6/11 3/7) 3)
    (check-equal? (round-complex #i1/3+17/5i) 0.+3.i)
    (check-equal? (round-complex 1/3+17/5i) 0+3i)
    (check-equal? (gcd-complex 0 4+2i) 4+2i)
    (check-equal? (gcd-complex 4+2i 0) 4+2i)
    (check-equal? (gcd-complex 52+30i 38-16i) -2+8i)
    (check-equal? (scheme-number-gcd 4. 8.) 1)
    (check-equal? (scheme-number-gcd 4 8) 4)
    (check-equal? (scheme-number-gcd 4/5 8/7) 4)
    (check-equal? (scheme-number-gcd 4+3i 8+16i) 2-i)
    (check-equal? (scheme-number-gcd 4+3i 8/5+16/5i) 2-i))
   (test-case
    "divide"
    (parameterize ([*no-rationals-in-divide* #t])
      (check-equal? (scheme-number-divide 4 3 list) '(1 1))
      (check-equal? (scheme-number-divide 4 3/2 list) '(8/3 0)))
    (parameterize ([*no-rationals-in-divide* #f])
      (check-equal? (scheme-number-divide 4 3 list) '(4/3 0))
      (check-equal? (scheme-number-divide 4 3/2 list) '(8/3 0))))
   (test-case
    "quad & cube"
    (define (L+ v) (λ _ (list* v _)))
    (define-values (TR CR DR L NS) (apply values (map L+ '(TR CR DR L NS))))
    (check-equal? (quadratic 1 0 -1 TR) '(TR -1 1))
    (check-equal? (quadratic 1 0 -1 TR CR DR L NS) '(TR -1 1))
    (check-equal? (quadratic 1 0 1 TR) '(TR 0-1i 0+1i))
    (check-equal? (quadratic 1 0 1 TR CR DR L NS) '(CR 0-1i 0+1i))
    (check-equal? (quadratic 1 0 0 TR) '(TR 0 0))
    (check-equal? (quadratic 1 0 0 TR CR DR L NS) '(DR 0))
    (check-exn #px"No solution -- QUADRATIC" (λ () (quadratic 0 0 1 TR)) )
    (check-equal? (quadratic 0 0 1 TR CR DR L NS) '(NS 0 0 1))
    (check-exn #px"Not QUADRATIC" (λ () (quadratic 0 1 1 TR)) )
    (check-equal? (quadratic 0 1 1 TR CR DR L NS) '(L -1))

    (check-within (sort (cubic -6 11 -6 list) < #:key real-part) '(1+0i 2+0i 3+0i) 5e-16)
    (check-within (sort (cubic 0 0 0 list) < #:key real-part) '(0 0 0) 5e-16))
   (test-case
    "softmax"
    (check-equal? (softmax (vector 4)) #(1.))
    (check-equal? (softmax (vector 2 2)) #(.5 .5))
    (check-true (apply < (vector->list (softmax (vector -4 2 3 4 5 6)))))
    (check-true (apply > (vector->list (softmax (vector 6 5 4 3 2 -4)))))
    (check-= (apply + (vector->list (softmax (vector 6 5 4 3 2 -4)))) 1. (* 3 *machine-epsilon*))
    (check-true (andmap (λ (x) (<= 0. x 1.)) (vector->list (softmax (vector 6 5 4 3 2 -4))))))
   
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))