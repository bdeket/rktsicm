#lang racket/base

(require rackunit
         "../../simplify/rcf.rkt"
         "../../simplify.rkt"
         "../helper.rkt")

(define P11 (poly/make 1 '(3 2 1)))
(define P12 (poly/make 1 '(2 1 3)))
(define P21 (poly/make 1 '(2 1 0)))
(define P22 (poly/make 1 '(1 2 0)))

(provide the-tests)
(define the-tests
  (test-suite
   "simplify/rcf"
   (test-case
    "ratform"
    (define T (make-ratform 'num 'den))
    (check-true (ratform? T))
    (check-equal? (ratform-numerator T) 'num)
    (check-equal? (ratform-denominator T) 'den))
   (test-case
    "rcf 0? 1?"
    (check-eq? rcf:zero poly:zero)
    (check-eq? rcf:one poly:one)
    (check-true  (rcf:zero? rcf:zero))
    (check-false (rcf:zero? rcf:one))
    (check-true  (rcf:one? rcf:one))
    (check-false (rcf:one? rcf:zero)))
   (test-case
    "rcf"
    (check-equal? (make-rcf poly:zero 'any) poly:zero)
    (check-equal? (make-rcf 'any poly:one) 'any)
    (define R (make-rcf P11 P12))
    (check-true (ratform? R))
    (check-equal? (rcf:numerator R) P11)
    (check-equal? (rcf:denominator R) P12)
    (define T (make-rcf (poly/make 1 '(3 2/3 0)) (poly/make 1 '(-2 1 0))))
    (check-true (ratform? T))
    (check-equal? (rcf:numerator T) (poly/make 1 '(-9 -2 0)))
    (check-equal? (rcf:denominator T) (poly/make 1 '(6 -3 0)))
    (check-equal? (rcf:numerator P11) P11)
    (check-equal? (rcf:denominator P11) poly:one)
    (check-exn #px"Zero denominator -- MAKE-RCF"
               (λ () (make-rcf poly:one poly:zero)))
    (check-exn #px"Wrong type -- NUMERATOR" (λ () (rcf:numerator 'wrong)))
    (check-exn #px"Wrong type -- DENOMINATOR" (λ () (rcf:denominator 'wrong))))
   (test-case
    "rcf? pcf?"
    (define R (make-rcf P11 P12))
    (check-true  (rcf:rcf? R))
    (check-true  (rcf:rcf? P11))
    (check-false (rcf:rcf? 'any))
    (check-false (rcf:pcf? R))
    (check-true  (rcf:pcf? P11))
    (check-false (rcf:pcf? 'any)))
   (test-case
    "rcf:arity"
    (check-equal? (rcf:arity (poly/make 1 '(-9 -2 0))) 1)
    (check-equal? (rcf:arity (poly/make 2 '(-9 -2 0))) 2)
    (check-equal? (rcf:arity 3) 0)
    (check-equal? (rcf:arity (make-rcf (poly/make 2 '(-9 -2 0)) (poly/make 2 '(-9 -2 0)))) 2)
    (check-equal? (rcf:arity (make-rcf (poly/make 2 '(-9 -2 0)) 3)) 2)
    (check-equal? (rcf:arity (make-rcf 3 (poly/make 1 '(-9 -2 0)))) 1)
    (check-equal? (rcf:arity (make-rcf 3 2)) 0)
    (check-exn #px"Unequal arities in RCF"
               (λ () (rcf:arity (make-rcf (poly/make 2 '(-9 -2 0)) (poly/make 1 '(-9 -2 0))))))
    (check-exn #px"Wrong type -- RCF:ARITY"
               (λ () (rcf:arity 'wrong))))
   (test-case
    "rcf:="
    (check-true  (rcf:= (make-rcf 3 4) (make-rcf 3/2 2)))
    (check-false (rcf:= (make-rcf 3 4) (make-rcf 3 3)))
    (check-true  (rcf:= (make-rcf P11 P12) (make-rcf P11 P12)))
    (skip ;; TODO : these should be ok
     (check-true (rcf:= (make-rcf P1 P2) (make-rcf (poly:* P1 2) (poly:* P2 2))))
     (check-true (rcf:= (make-rcf P1 2) (poly:* P1 1/2)))
     (check-true (rcf:= (poly:* P1 1/2) (make-rcf P1 2))))
    (check-false (rcf:= (make-rcf P11 2) P11))
    (check-false (rcf:= P11 (make-rcf P11 2)))
    (check-true  (rcf:= P11 P11))
    (check-false (rcf:= P11 P12))
    (check-exn #px"Wrong type -- RCF:= "
               (λ () (rcf:= (make-rcf P11 P12) 'wrong)))
    (check-exn #px"Wrong type -- RCF:= "
               (λ () (rcf:= 'wrong (make-rcf P11 P12)))))
   (test-case
    "rcf:binary-operator"
    (define u/u* (make-ratform 'u 'u*))
    (define v/v* (make-ratform 'v 'v*))
    (define (mk x) (λ _ (list* x _)))
    (check-equal? (rcf:binary-operator u/u* v/v* (mk 'ii) (mk 'ir) (mk 'ri) (mk 'rr))
                  '(rr u u* v v*))
    (check-equal? (rcf:binary-operator u/u* v/v* (mk 'ii) (mk 'ir) (mk 'ri) (mk 'rr))
                  '(rr u u* v v*))
    (check-equal? (rcf:binary-operator u/u* 'v/v* (mk 'ii) (mk 'ir) (mk 'ri) (mk 'rr))
                  '(ri u u* v/v*))
    (check-equal? (rcf:binary-operator 'u/u* v/v* (mk 'ii) (mk 'ir) (mk 'ri) (mk 'rr))
                  '(ir u/u* v v*))
    (check-equal? (rcf:binary-operator 'u/u* 'v/v* (mk 'ii) (mk 'ir) (mk 'ri) (mk 'rr))
                  '(ii u/u* v/v*)))
   (test-case
    "rcf:+"
    (check-equal? (rcf:+ P11 P21) (poly/make 1 '(5 3 1)))
    (check-equal? (rcf:+ poly:zero (make-rcf P21 P22)) (make-rcf P21 P22))
    (check-equal? (rcf:+ P11 (make-rcf P21 P22))
                  (make-rcf (poly/make 1 '(3 8 7 3 0)) P22))
    (check-equal? (rcf:+ (make-rcf P21 P22) poly:zero) (make-rcf P21 P22))
    (check-equal? (rcf:+ (make-rcf P21 P22) P11)
                  (make-rcf (poly/make 1 '(3 8 7 3 0)) P22))
    (check-equal? (rcf:+ (make-rcf P11 P12) (make-rcf P21 P12))
                  (make-rcf (poly/make 1 '(5 3 1))
                            P12))
    (check-equal? (rcf:+ (make-rcf (poly/make 1 '(2 -1 0)) P22) (make-rcf P21 P22))
                  (make-rcf (poly/make-from-sparse 1 '((1 . 4)))
                            (poly/make 1 '(1 2))))
    ;; TODO : x/x can be factored out... should it be?
    (check-equal? (rcf:+ (make-rcf P11 P12) (make-rcf P21 P22))
                  (make-rcf (poly/make 1 '(7 12 12 5 0))
                            (poly/make 1 '(2 5 5 6 0))))
    (check-equal? (rcf:+ (make-rcf (poly/make 1 '(6 -5 -4)) P21)
                         (make-rcf (poly/make 1 `(-3 -2 8)) P22))
                  poly:zero)
    (check-equal? (rcf:+ (make-rcf P11 P21) (make-rcf P12 P22))
                  (make-rcf (poly/make 1 '(7 12 12 5))
                            (poly/make 1 '(2 5 2 0))))
    (check-equal? (rcf:+ (make-rcf (poly/make 1 '(6 -3 -4)) P21)
                         (make-rcf (poly/make 1 `(-3 -2 8)) P22))
                  (make-rcf (poly/make 1 '(2 4))
                            (poly/make 1 '(2 5 2)))))
   (test-case
    "rcf:-"
    (check-equal? (rcf:- P11 P21) (poly/make 1 '(1 1 1)))
    (check-equal? (rcf:- poly:zero (make-rcf P21 P22)) (make-rcf (poly:* -1 P21) P22))
    (check-equal? (rcf:- P11 (make-rcf P21 P22))
                  (make-rcf (poly/make 1 '(3 8 3 1 0)) P22))
    (check-equal? (rcf:- (make-rcf P21 P22) poly:zero) (make-rcf P21 P22))
    (check-equal? (rcf:- (make-rcf P21 P22) P11)
                  (make-rcf (poly/make 1 '(-3 -8 -3 -1 0)) P22))
    (check-equal? (rcf:- (make-rcf P11 P12) (make-rcf P21 P12))
                  (make-rcf (poly/make 1 '(1 1 1))
                            P12))
    (check-equal? (rcf:- (make-rcf (poly/make 1 '(2 -1 0)) P22) (make-rcf P21 P22))
                  (make-rcf -2 (poly/make 1 '(1 2))))
    ;; TODO : x/x can be factored out... should it be?
    (check-equal? (rcf:- (make-rcf P11 P12) (make-rcf P21 P22))
                  (make-rcf (poly/make 1 '(-1 4 -2 -1 0))
                            (poly/make 1 '(2 5 5 6 0))))
    (check-equal? (rcf:- (make-rcf (poly/make 1 '(6 -5 -4)) P21)
                         (make-rcf (poly/make 1 `(3 2 -8)) P22))
                  poly:zero)
    (check-equal? (rcf:- (make-rcf P11 P21) (make-rcf P12 P22))
                  (make-rcf (poly/make 1 '(-1 4 -2 -1))
                            (poly/make 1 '(2 5 2 0))))
    (check-equal? (rcf:- (make-rcf (poly/make 1 '(6 -3 -4)) P21)
                         (make-rcf (poly/make 1 `(3 2 -8)) P22))
                  (make-rcf (poly/make 1 '(2 4))
                            (poly/make 1 '(2 5 2))))

    (check-equal? (rcf:negate (make-rcf P11 P12)) (make-rcf (poly:* -1 P11) P12))
    (check-equal? (rcf:negate P11) (poly:* -1 P11)))
   (test-case
    "rcf:*"
    (check-equal? (rcf:* P11 P21) (poly/make 1 '(6 7 4 1 0)))
    (check-equal? (rcf:* poly:zero (make-rcf P21 P22)) poly:zero)
    (check-equal? (rcf:* poly:one  (make-rcf P21 P22)) (make-rcf P21 P22))
    (check-equal? (rcf:* P11 (make-rcf P21 P22)) ;; TODO : x/x can be factored out...
                  (make-rcf (poly/make 1 '(6 7 4 1 0)) P22))
    (check-equal? (rcf:* P21 (make-rcf P11 P22)) ;; and here it is
                  (make-rcf (poly/make 1 '(6 7 4 1)) (poly/make 1 '(1 2))))
    (check-equal? (rcf:* (make-rcf P21 P22) poly:zero) poly:zero)
    (check-equal? (rcf:* (make-rcf P21 P22) poly:one) (make-rcf P21 P22))
    (check-equal? (rcf:* (make-rcf P21 P22) P11) ;; TODO : x/x can be factored out...
                  (make-rcf (poly/make 1 '(6 7 4 1 0)) P22))
    (check-equal? (rcf:* (make-rcf P11 P22) P21) ;; and here it is
                  (make-rcf (poly/make 1 '(6 7 4 1)) (poly/make 1 '(1 2))))
    (check-equal? (rcf:* (make-rcf P11 P12) (make-rcf P21 P12))
                  (make-rcf (poly/make 1 '(6 7 4 1 0))
                            (poly/make 1 '(4 4 13 6 9))))
    (check-equal? (rcf:* (make-rcf (poly/make 1 '(2 -1 0)) P22) (make-rcf P21 P22))
                  (make-rcf (poly/make 1 '(4 0 -1))
                            (poly/make 1 '(1 4 4))))
    ;; TODO : x/x can be factored out... should it be?
    (check-equal? (rcf:* (make-rcf P11 P12) (make-rcf P21 P22))
                  (make-rcf (poly/make 1 '(6 7 4 1 0))
                            (poly/make 1 '(2 5 5 6 0))))
    (check-equal? (rcf:* (make-rcf P11 P22) (make-rcf P21 P22))
                  (make-rcf (poly/make 1 '(6 7 4 1))
                            (poly/make 1 '(1 4 4 0))))
    (check-equal? (rcf:* (make-rcf P21 P22) (make-rcf P11 P22))
                  (make-rcf (poly/make 1 '(6 7 4 1))
                            (poly/make 1 '(1 4 4 0))))

    (check-equal? (rcf:square P11) (rcf:* P11 P11))
    (check-equal? (rcf:square (make-rcf P11 P12)) (rcf:* (make-rcf P11 P12) (make-rcf P11 P12))))
   (test-case
    "rcf:/"
    (check-equal? (rcf:invert P11) (make-rcf 1 P11))
    (check-equal? (rcf:invert (make-rcf P11 P12))
                  (make-rcf P12 P11))
    (check-equal? (rcf:/ P11 P12) (make-rcf P11 P12))
    (check-equal? (rcf:/ (make-rcf P11 P12) (make-rcf P21 P22))
                  (make-rcf (poly/make 1 '(3 8 5 2 0))
                            (poly/make 1 '(4 4 7 3 0)))))
   (test-case
    "rcf:gcd"
    (check-equal? (rcf:gcd P21 P22) (poly/make 1 '(1 0)))
    
    (check-equal? (rcf:gcd poly:zero (make-rcf P11 P22)) (make-rcf P11 P22))
    (check-equal? (rcf:gcd poly:one  (make-rcf P11 P22)) 1)
    (check-equal? (rcf:gcd P21 (make-rcf P11 P22)) 1)
    (check-equal? (rcf:gcd P21 (make-rcf P22 P11)) (poly/make 1 '(1 0)))

    (check-equal? (rcf:gcd (make-rcf P11 P22) poly:zero) (make-rcf P11 P22))
    (check-equal? (rcf:gcd (make-rcf P11 P22) poly:one) 1)
    (check-equal? (rcf:gcd (make-rcf P11 P22) P21) 1)
    (check-equal? (rcf:gcd (make-rcf P22 P11) P21) (poly/make 1 '(1 0)))

    (check-equal? (rcf:gcd (make-rcf P11 P12) (make-rcf P21 P22))
                  1)
    (check-equal? (rcf:gcd (make-rcf (poly:* 3 P11) P21) (make-rcf (poly:* 3 P12) P22))
                  (make-rcf 3 (poly/make 1 '(1 0)))))
   (test-case
    "rcf:expt"
    (check-equal? (rcf:expt (make-rcf P11 P12) 0) 1)
    (check-equal? (rcf:expt (make-rcf P11 P12) 1) (make-rcf P11 P12))
    (check-equal? (rcf:expt (make-rcf P11 P12) 2) (rcf:square (make-rcf P11 P12)))
    (check-equal? (rcf:expt (make-rcf P11 P12) 3) (rcf:* (make-rcf P11 P12) (rcf:square (make-rcf P11 P12))))
    
    (check-equal? (rcf:expt (make-rcf P11 P12) -1) (make-rcf P12 P11))
    (check-equal? (rcf:expt (make-rcf P11 P12) -2) (rcf:square (make-rcf P12 P11)))
    (check-equal? (rcf:expt (make-rcf P11 P12) -3) (rcf:* (make-rcf P12 P11) (rcf:square (make-rcf P12 P11))))

    (check-exn #px"Can only raise a RCF to an exact integer power"
               (λ () (rcf:expt (make-rcf P11 P12) 1.5))))

   (test-case
    "rcf:arg-scale / shift"
    (check-equal? (rcf:arg-scale (make-rcf P11 P12) '(1/3))
                  (make-rcf (poly/make 1 '(3 6 9))
                            (poly/make 1 '(2 3 27))))
    (check-equal? (rcf:arg-scale (poly/make 2 `(3 ,(poly/make 1 '(1 0)) 9)) '(1/3 2))
                  (poly/make 2 `(1/3 ,(poly/make 1 '(2/3 0)) 9)))

    (check-equal? (rcf:arg-shift (make-rcf P11 P12) '(3))
                  (make-rcf (poly/make 1 '(3 20 34))
                            (poly/make 1 '(2 13 24))))
    (check-equal? (rcf:arg-shift (poly/make 2 `(3 ,(poly/make 1 '(1 0)) 9)) '(1/3 2))
                  (poly/make 2 `(3 ,(poly/make 1 '(1 4)) ,(poly/make 1 '(1/3 10))))))
   (test-case
    "rcf:value"
    ;; TODO :  is it really best to return a rcf? for numerical values?
    (check-equal? (rcf:value (make-rcf P11 P12) '(1/3)) (make-rcf 9 16))
    (check-equal? (rcf:value (poly/make 2 `(3 ,(poly/make 1 '(1 0)) 9)) '(1/3 2))
                  10)
    (check-equal? (rcf:value (make-rcf P11 P12) (list (poly/make 1 '(1 0 0))))
                  (make-rcf (poly/make 1 '(3 0 2 0 1)) (poly/make 1 '(2 0 1 0 3)))))
   (test-case
    "rcf:compose"
    (check-equal? (rcf:compose (make-rcf P11 P12) 1/3) (make-rcf 9 16))
    (check-equal? (rcf:compose (make-rcf P11 P12) (poly/make 1 '(1 0 0)))
                  (make-rcf (poly/make 1 '(3 0 2 0 1)) (poly/make 1 '(2 0 1 0 3))))
    (check-equal? (rcf:compose (make-rcf P11 P12) (make-rcf P21 P22))
                  (make-rcf (poly/make 1 '(17 26 11)) (poly/make 1 '(13 25 16))))
    (check-equal? (rcf:compose (make-rcf (poly/make 1 '(3 2 1 0)) P12) (make-rcf P21 P22))
                  (make-rcf (poly/make 1 '(17 26 11)) (poly/make 1 '(13 51 66 32 0))))
    (check-equal? (rcf:compose (make-rcf (poly/make 1 '(3 2)) P12) (make-rcf P21 P22))
                  (make-rcf (poly/make 1 '(8 23 14)) (poly/make 1 '(13 25 16)))))
   (test-case
    "rcf:derivative"
    (check-equal? (rcf:derivative P11 1) (poly/make 1 '(6 2)))
    (check-equal? (rcf:derivative (make-rcf P11 P12) 1)
                  (make-rcf (poly/make 1 '(-1 14 5))
                            (poly/make 1 '(4 4 13 6 9))))
    (check-equal? (rcf:derivative (make-rcf (poly/make 2 `(-1 ,(poly/make 1 '(1 0)) 5))
                                            (poly/make 2 '(2 1 3)))
                                  #;(/ (+ (* -1 (expt x 2)) (* x y) 5)
                                       (+ (* 2 (expt x 2)) x 3))
                                  1)
                  (make-rcf (poly/make 2 `(,(poly/make 1 '(-2 -1)) -26 ,(poly/make 1 '(3 -5))))
                            (poly/make 2 '(4 4 13 6 9)))
                  #;(/ (+ (* -2 (expt x 2) y) (* -1 (expt x 2)) (* -26 x) (* 3 y) -5)
                       (+ (* 4 (expt x 4)) (* 4 (expt x 3)) (* 13 (expt x 2)) (* 6 x) 9)))
    (check-equal? (rcf:derivative (make-rcf (poly/make 2 `(-1 ,(poly/make 1 '(1 0)) 5))
                                            (poly/make 2 '(2 1 3)))
                                  #;(/ (+ (* -1 (expt x 2)) (* x y) 5)
                                       (+ (* 2 (expt x 2)) x 3))
                                  2)
                  (make-rcf (poly/make-from-sparse 2 `((1 . 1)))
                            (poly/make 2 '(2 1 3)))
                  #;(/ x
                       (+ (* 2 (expt x 2)) x 3))))
   (test-case
    "assoc-accumulation / inverse"
    (define (mk op) (λ _ (list* op _)))
    (check-equal? ((assoc-accumulation (mk 'rat) (mk 'pol) 'idty))
                  'idty)
    (check-equal? ((assoc-accumulation (mk 'rat) (mk 'pol) 'idty) 'any)
                  'any)
    (check-equal? ((assoc-accumulation (mk 'rat) (mk 'pol) 'idty) 'pol1 'pol2)
                  '(pol pol1 pol2))
    (define R1 (make-ratform 'n1 'd1))
    (define R2 (make-ratform 'n2 'd2))
    (define R3 (make-ratform 'n3 'd3))
    (check-equal? ((assoc-accumulation (mk 'rat) (mk 'pol) 'idty) 'pol1 R1)
                  `(rat pol1 ,R1))
    (check-equal? ((assoc-accumulation (mk 'rat) (mk 'pol) 'idty) R1 'pol1)
                  `(rat ,R1 pol1))
    (check-equal? ((assoc-accumulation (mk 'rat) (mk 'pol) 'idty) R1 R2)
                  `(rat ,R1 ,R2))
    (check-equal? ((assoc-accumulation (mk 'rat) (mk 'pol) 'idty) R1 R2 R3)
                  `(rat (rat ,R1 ,R2) ,R3))
    (check-equal? ((assoc-accumulation (mk 'rat) (mk 'pol) 'idty) R1 R2 'pol1)
                  `(pol (rat ,R1 ,R2) pol1))
    (check-equal? ((assoc-accumulation (mk 'rat) (mk 'pol) 'idty) R1 'pol1 R2)
                  `(rat (rat ,R1 pol1) ,R2))
    (check-equal? ((assoc-accumulation (mk 'rat) (mk 'pol) 'idty) R1 'pol1 'pol2)
                  `(rat ,R1 (pol pol1 pol2)))


    (check-equal? ((assoc-inverse-accumulation (mk 'rat-1) (mk 'rat) (mk 'r-inv) (mk 'pol) 'idt))
                  'idt)
    (check-equal? ((assoc-inverse-accumulation (mk 'rat-1) (mk 'rat) (mk 'r-inv) (mk 'pol) 'idt) 'R0)
                  '(r-inv R0))
    (check-equal? ((assoc-inverse-accumulation (mk 'rat-1) (mk 'rat) (mk 'r-inv) (mk 'pol) 'idt) R1 'pol1 'pol2 R2)
                  `(rat-1 ,R1 (rat (pol pol1 pol2) ,R2))))
   (test-case
    "$rcf + - * /"
    (check-equal? (+$rcf (make-rcf P11 P12) 3 (make-rcf P21 P22) P22 5)
                  (make-rcf (poly/make 1 '(2 9 38 68 64 53 0))
                            (poly/make 1 '(2 5 5 6 0))))
    (check-equal? (-$rcf (make-rcf P11 P12) 3 (make-rcf P21 P22) P22 5)
                  (make-rcf (poly/make 1 '(-2 -9 -32 -52 -54 -49 0))
                            (poly/make 1 '(2 5 5 6 0))))
    (check-equal? (*$rcf (make-rcf P11 P12) 3 (make-rcf P21 P22) P22 5)
                  (make-rcf (poly/make 1 '(90 105 60 15 0))
                            (poly/make 1 '(2 1 3))))
    (check-equal? (/$rcf (make-rcf P11 P12) 3 (make-rcf P21 P22) P22 5)
                  (make-rcf (poly/make 1 '(3 2 1))
                            (poly/make 1 '(60 60 105 45 0)))))
   (test-case
    "rcf:<->expression"
    (check-equal? (rcf:->expression P11 '(x))
                  '(+ 1 (* (+ 2 (* 3 x)) x)))
    (check-equal? (rcf:->expression (make-rcf P11 P12) '(x))
                  '(/ (+ 1 (* (+ 2 (* 3 x)) x)) (+ 3 (* (+ 1 (* 2 x)) x))))
    (check-equal? (rcf:->expression (poly/make 2 `(,(poly/make 1 '(1 0 0)) 3 2)) '(x y))
                  '(+ 2 (* (+ 3 (* (expt y 2) x)) x)))
    
    (check-equal? (rcf:expression-> '(/ (+ 1 (* 2 x) (* 3 (expt x 2))) (- 3 (negate (* (+ 1 (* 2 x)) x)))) vector)
                  (vector (make-rcf P11 P12) '(x)))
    (check-equal? (rcf:expression-> '(* (+ 1 (* 2 x) (* 3 (square x))) (invert (+ 3 (* (+ 1 (* 2 x)) x)))) vector)
                  (vector (make-rcf P11 P12) '(x)))
    (check-equal? (rcf:expression-> '(+ 2 (* (+ 3 (* (expt y 2) x)) x)) vector)
                  (vector (poly/make 2 `(,(poly/make-from-sparse 1 '((2 . 1))) 3 2))
                          '(x y)))
    (check-equal? (rcf:expression-> '(+ 2 (* (+ 3 (* (expt y 2) x)) x)) vector (λ (a b) (string-ci>? (format "~a" a) (format "~a" b))))
                  (vector (poly/make 2 `(,(poly/make-from-sparse 1 '((2 . 1))) 0 ,(poly/make 1 '(3 2))))
                          '(y x)))
    (check-equal? (rcf:expression-> '(gcd (+ 1 (* 2 x) (* 3 (square x))) (+ 3 (* (+ 1 (* 2 x)) x))) vector)
                  (vector 1 '(x))))
   (test-case
    "rcf:->lambda"
    (check-equal? (rcf:->lambda (make-rcf P11 P12))
                  '(lambda (x.0) (/ (+ 1 (* (+ 2 (* 3 x.0)) x.0))
                                    (+ 3 (* (+ 1 (* 2 x.0)) x.0)))))
    (check-equal? (rcf:->lambda P11)
                  '(lambda (x.0) (+ 1 (* (+ 2 (* 3 x.0)) x.0)))))

   (test-case
    "todo"
    rcf:->lambda
    )
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))