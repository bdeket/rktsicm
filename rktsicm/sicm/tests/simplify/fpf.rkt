#lang racket/base

(require rackunit
         "../../simplify/fpf.rkt"
         "../helper.rkt")

(define (mkTerms . a) (map (λ (x) (fpf:make-term (car x) (cadr x))) a))
(define (randExp [n 5] [m 20]) (build-list n (λ _ (random m))))
(define (randTerm [n 5] [m 20] [t 50])
  (define T (if (number? t)
                (if (exact? t) (λ () (- (random (* 2 t)) t)) (λ () (- (* 2 t (random)) t)))
                t))
  (fpf:make-term (randExp n m) (T)))
(define (randFPF [r 5] [n 5] [m 20] [t 50])
  (fpf:clean (fpf:make (build-list r (λ _ (randTerm n m t))))))
(define (fpf:clean F)
  (for/fold ([H #hash()]
             #:result (fpf:make (sort (for/list ([(k v) (in-hash H)] #:unless (= v 0)) (fpf:make-term k v)) fpf:>exponents? #:key fpf:exponents)))
            ([t (in-list (fpf:terms F))])
    (hash-update H (fpf:exponents t) (λ (x) (+ x (fpf:coefficient t))) 0)))

(provide the-tests)
(define the-tests
  (test-suite
   "simplify/fpf"
   (test-case
    "basics"
    (check-true  (fpf:coeff? 5))
    (check-false (fpf:coeff? 'a))
    (check-true  (fpf:coeff-zero? 0))
    (check-false (fpf:coeff-zero? 1))
    (check-equal? (fpf:coeff-add 1 2) 3)
    (check-equal? (fpf:coeff-sub 1 2) -1)
    (check-equal? (fpf:coeff-mul 1 2) 2)
    (check-equal? (fpf:coeff-div 1 2) 1/2)
    (check-equal? (fpf:coeff-negate 5) (- 5))
    (check-equal? (fpf:coeff-expt 5 2) 25)
    (check-equal? (fpf:coeff-divide 5 2 list) '(5/2 0)))
   (test-case
    "terms"
    (define T (fpf:make-term '(1 2 3) 4))
    (check-equal? (fpf:exponents T) '(1 2 3))
    (check-equal? (fpf:coefficient T) 4)
    (check-true  (all-zeros? '()))
    (check-true  (all-zeros? '(0)))
    (check-true  (all-zeros? '(0 0 0)))
    (check-false (all-zeros? '(0 0 1 0)))
    (check-true  (fpf:constant-term? (fpf:make-term '(0 0 0) 3)))
    (check-false (fpf:constant-term? (fpf:make-term '(0 0 1) 0))))
   (test-case
    "fpf"
    (define P0 (fpf:make '()))
    (define P1 (fpf:make '(((0) . 5))))
    (define P2 (fpf:make '(((1) . 7))))
    (define P3 (fpf:make-constant 3 3))
    (check-true  (fpf? 5))
    (check-true  (fpf? P0))
    (check-true  (fpf? P1))
    (check-true  (fpf? P2))
    (check-true  (fpf? P3))
    (check-false (fpf? 'a))
    (check-equal? (fpf:terms P0) '())
    (check-equal? (fpf:terms P1) '((() . 5)))
    (check-equal? (fpf:terms P2) '(((1) . 7)))
    (check-equal? (fpf:terms P3) '(((0 0 0) . 3)))

    (check-false (explicit-fpf? P0))
    (check-false (explicit-fpf? P1))
    (check-true  (explicit-fpf? P2))
    (check-true  (explicit-fpf? P3))
    (check-true  (explicit-fpf? (fpf:make-explicit '(((1) . 1) ((2) . 1) ((3) . 5)))))
    (check-true  (explicit-fpf? (fpf:make-explicit '((1) . 1)))))
   (test-case
    "arity"
    (check-equal? (fpf:arity 0) 0)
    (check-equal? (fpf:arity 5) 0)
    (check-equal? (fpf:arity (fpf:make-constant 1 3)) 3)
    (check-equal? (fpf:arity (fpf:make-explicit (mkTerms '((1 2 3 4) 7)))) 4)
    (check-equal? (fpf:number-of-vars (fpf:terms (fpf:make-constant 1 3))) 3)
    (check-equal? (fpf:number-of-vars (fpf:terms (fpf:make-explicit (mkTerms '((1 2 3 4) 7))))) 4))
   (test-case
    "new-variables"
    (define n (+ (random 10) 1))
    (define P (fpf:new-variables n))
    (check-equal? (length P) n)
    (check-true (andmap fpf? P))
    (check-true (andmap (λ (x) (= (fpf:arity x) n)) P))
    (check-true (andmap (λ (x) (= (length (fpf:terms x)) 1)) P)))
   (test-case
    "fpf:same-exponents"
    ;; works on the exponents of a term
    (check-true  (fpf:same-exponents? '() '()))
    (check-true  (fpf:same-exponents? '(1 2 5) '(1 2 5)))
    (check-false (fpf:same-exponents? '(1 2 5) '(1 2 5 6)))
    (check-false (fpf:same-exponents? '(1 2 5) '(1 2 4))))
   (test-case
    "exponents >"
    (check-false (fpf:lexicographical> '() '()))
    (check-false (fpf:lexicographical> '() '(1)))
    (check-true  (fpf:lexicographical> '(1) '()))
    (check-true  (fpf:lexicographical> '(2) '(1)))
    (check-false (fpf:lexicographical> '(1) '(2)))
    (check-false (fpf:lexicographical> '(2) '(2)))
    (check-true  (fpf:lexicographical> '(2 1) '(2)))
    (check-false (fpf:lexicographical> '(2 1) '(2 1)))
    (check-false (fpf:lexicographical> '(0 2 3) '(1 2)))

    (check-false (fpf:graded> '() '()))
    (check-false (fpf:graded> '() '(1)))
    (check-true  (fpf:graded> '(1) '()))
    (check-true  (fpf:graded> '(2) '(1)))
    (check-false (fpf:graded> '(1) '(2)))
    (check-false (fpf:graded> '(2) '(2)))
    (check-true  (fpf:graded> '(2 1) '(2)))
    (check-false (fpf:graded> '(2 1) '(2 1)))
    (check-true  (fpf:graded> '(0 2 3) '(1 2)))

    (let ([fs1 (randExp (random 10))]
          [fs2 (randExp (random 10))])
      (check-equal? (fpf:graded> fs1 fs2) (fpf:>exponents? fs1 fs2))))
   (test-case
    "fpf:map-coefficients"
    (check-equal? (fpf:map-coefficients error '()) '())
    (check-equal? (fpf:map-coefficients add1 (mkTerms '((3) -1))) '())
    (check-equal? (fpf:map-coefficients add1 (mkTerms '((3) 0)))
                  (mkTerms '((3) 1)))
    (check-equal? (fpf:map-coefficients add1 (mkTerms '((0 3) 0) '((3 2) 7)))
                  (mkTerms '((0 3) 1) '((3 2) 8)))
    (check-equal? (fpf:map-coefficients add1 (mkTerms '((0 3) 0) '((1 1) -1) '((3 2) 7)))
                  (mkTerms '((0 3) 1) '((3 2) 8))))

   (test-case
    "fpf:binary-combine"
    (check-equal? (fpf:binary-combine 3 4 vector append 'test) (vector 3 4))
    (check-equal? (fpf:binary-combine 3 (fpf:make-constant 4 2) vector append 'test)
                  (fpf:make (append (fpf:terms (fpf:make-constant 3 2)) (fpf:terms (fpf:make-constant 4 2)))))
    (check-exn #px"Wrong type argument -- FPF 'test"
               (λ () (fpf:binary-combine 3 'wrong vector append 'test)))
    (check-equal? (fpf:binary-combine (fpf:make-constant 4 2) 3 vector append 'test)
                  (fpf:make (append (fpf:terms (fpf:make-constant 4 2)) (fpf:terms (fpf:make-constant 3 2)))))
    (check-exn #px"Wrong type argument -- FPF 'test"
               (λ () (fpf:binary-combine 'wrong 3 vector append 'test)))
    (check-equal? (fpf:binary-combine (fpf:make-constant 4 2) (fpf:make-constant 5 2) vector append 'test)
                  (fpf:make (append (fpf:terms (fpf:make-constant 4 2)) (fpf:terms (fpf:make-constant 5 2)))))
    (check-exn #px"Wrong type argument -- FPF 'test"
               (λ () (fpf:binary-combine 'wrong 'worse vector append 'test))))
   (test-case
    "fpf:+"
    ;; algorithm assumes that terms are sorted in exponent> order
    (define P0 (fpf:make-constant 3 2))
    (define P1 (fpf:make-constant 3 3))
    (check-equal? (fpf:add-terms-general '() 'wrong -) 'wrong)
    (check-equal? (fpf:add-terms-general 'wrong '() -) 'wrong)
    (check-equal? (fpf:add-terms-general (fpf:terms P0) (fpf:terms P0) +)
                  (fpf:terms (fpf:make-constant 6 2)))
    (check-equal? (fpf:add-terms-general (fpf:terms P0) (fpf:terms P0) -)
                  '())
    (check-equal? (fpf:add-terms-general (fpf:terms P0) (fpf:terms P1) +)
                  (append (fpf:terms P1) (fpf:terms P0)))
    (check-equal? (fpf:add-terms-general (mkTerms '((2 1) 3) '((0 1) 2))
                                         (mkTerms '((0 1) 3) '((2 1) 2)) +)
                  ;;TODO ;; this is not very good - how/where to make sure terms are exponent>?
                  (mkTerms '((2 1) 3) '((0 1) 5) '((2 1) 2)))
    (check-equal? (fpf:+ 3 4) 7)
    (check-equal? (fpf:+ 3 (fpf:make-constant 3 2)) 6)
    (check-equal? (fpf:+ (fpf:make (mkTerms '((2 1) 3) '((0 1) 2)))
                         (fpf:make (mkTerms '((0 1) 3) '((2 1) 2))))
                  (fpf:make (mkTerms '((2 1) 3) '((0 1) 5) '((2 1) 2))))
    (check-exn #px"Wrong type argument -- FPF 'add"
               (λ () (fpf:+ 3 'wrong))))
   (test-case
    "fpf:scale"
    (check-equal? (fpf:scale-terms-general 3 (mkTerms '((2 1) 3) '((0 1) 2)) *)
                  (mkTerms '((2 1) 9) '((0 1) 6)))
    (check-equal? (fpf:scale-terms-general 3 (mkTerms '((2 1) 3) '((0 1) 2)) /)
                  (mkTerms '((2 1) 1) '((0 1) 3/2)))
    (check-equal? (fpf:scale 3 (fpf:make (mkTerms '((2 1) 3) '((0 1) 2))))
                  (fpf:make (mkTerms '((2 1) 9) '((0 1) 6))))
    (check-equal? (fpf:scale 3 7) 21))
   (test-case
    "fpf:negate"
    (check-equal? (fpf:negate-terms-general (mkTerms '((2 1) 3) '((0 1) 2)) -)
                  (mkTerms '((2 1) -3) '((0 1) -2)))
    (check-equal? (fpf:negate-terms-general (mkTerms '((2 1) 3) '((0 1) 2)) /)
                  (mkTerms '((2 1) 1/3) '((0 1) 1/2)))

    (check-equal? (fpf:negate (fpf:make (mkTerms '((2 1) 3) '((0 1) 2))))
                  (fpf:make (mkTerms '((2 1) -3) '((0 1) -2))))
    (check-equal? (fpf:negate 7) -7)
    (check-equal? (fpf:- 3 4) -1)
    (check-equal? (fpf:- 3 (fpf:make-constant 3 2)) 0)
    (check-equal? (fpf:- (fpf:make (mkTerms '((2 1) 3) '((0 1) 2)))
                         (fpf:make (mkTerms '((0 1) 3) '((2 1) 2))))
                  (fpf:make (mkTerms '((2 1) 3) '((0 1) -1) '((2 1) -2)))))
   (test-case
    "fpf:*"
    (check-equal? (fpf:combine-exponents '() '()) '())
    (check-equal? (fpf:combine-exponents '() '(1 2 3)) '(1 2 3))
    (check-equal? (fpf:combine-exponents '(1 2 3) '()) '(1 2 3))
    (check-equal? (fpf:combine-exponents '(1 2 3) '(3 2 1)) '(4 4 4))
    (check-exn #px"" (λ () (fpf:combine-exponents '(1 2 3) '(3 2 1 0))))

    (check-equal? (fpf:term*terms-general (fpf:make-term '(1 2) 3) '() vector)  '())
    (check-equal? (fpf:term*terms-general (fpf:make-term '(1 2) 3)
                                          (mkTerms '((0 2) 2) '((1 1) 4))
                                          vector)
                  (mkTerms '((1 4) #(3 2)) '((2 3) #(3 4))))

    ;; terms are expected to be sorted!
    (check-equal? (fpf:mul-terms-general '() '() make-rectangular +) '())
    (check-equal? (fpf:mul-terms-general (mkTerms '((1 2) 3)) '() make-rectangular +) '())
    (check-equal? (fpf:mul-terms-general '() (mkTerms '((1 2) 3)) make-rectangular +) '())
    (check-equal? (fpf:mul-terms-general  (mkTerms '((1 2) 3))
                                          (mkTerms '((2 1) 4))
                                          make-rectangular +)
                  (mkTerms '((3 3) 7)))
    (check-equal? (fpf:mul-terms-general  (mkTerms '((1 2) 3) '((0 2) 1))
                                          (mkTerms '((3 1) 3) '((2 1) 4))
                                          make-rectangular +)
                  (mkTerms '((4 3) 6) '((3 3) 7+4i) '((2 3) 5)))
    ;; wrong: input not sorted
    (check-equal? (fpf:mul-terms-general  (mkTerms '((1 2) 3) '((0 2) 1))
                                          (mkTerms '((2 1) 4) '((3 1) 3))
                                          make-rectangular +)
                  (mkTerms '((3 3) 7) '((4 3) 6) '((2 3) 5) '((3 3) 4)))

    (check-equal? (fpf:* (fpf:make (mkTerms '((1 2) 3) '((0 2) 1)))
                         (fpf:make (mkTerms '((3 1) 3) '((2 1) 4))))
                  (fpf:make (mkTerms '((4 3) 9) '((3 3) 15) '((2 3) 4))))
    (check-exn #px"Wrong type argument -- FPF 'mul"
               (λ () (fpf:* 3 'wrong)))

    (check-equal? (fpf:square (fpf:make (mkTerms '((2 1) 3) '((1 2) 1))))
                  (fpf:make (mkTerms '((4 2) 9) '((3 3) 6) '((2 4) 1)))))
   (test-case
    "fpf:expt"
    (check-equal? (fpf:expt 4 2) 16)
    ;; TODO: better error?
    (check-exn #px"expt: contract violation" (λ() (fpf:expt 4 (fpf:make (mkTerms '((2 1) 3))))))
    (check-exn #px"Wrong type -- FPF:EXPT:"
               (λ () (fpf:expt 'wrong 1.5)))
    (check-exn #px"Can only raise an FPF to an exact integer power"
               (λ () (fpf:expt (fpf:make (mkTerms '((2 1) 3) '((1 2) 1))) 1.5)))
    (check-exn #px"No inverse -- FPF:EXPT:"
               (λ () (fpf:expt (fpf:make (mkTerms '((2 1) 3) '((1 2) 1))) -3)))
    (check-equal? (fpf:expt (fpf:make (mkTerms '((2 1) 3) '((1 2) 1))) 1)
                  (fpf:make (mkTerms '((2 1) 3) '((1 2) 1))))
    (check-equal? (fpf:expt (fpf:make (mkTerms '((2 1) 3) '((1 2) 1))) 2)
                  (fpf:make (mkTerms '((4 2) 9) '((3 3) 6) '((2 4) 1)))))
   (test-case
    "fpf:divide"
    (check-equal? (fpf:divide-terms-general '() (mkTerms '((0 1) 2)) + * / - vector)
                  (vector '() '()))
    (check-equal? (fpf:divide-terms-general (mkTerms '((1 2) 3)) (mkTerms '((0 1) 2)) + * / - vector)
                  (vector (mkTerms '((1 1) 3/2)) '()))
    (check-equal? (fpf:divide-terms-general (mkTerms '((1 2) 3)) (mkTerms '((0 2) 2)) + * / - vector)
                  (vector (mkTerms '((1 0) 3/2)) '()))
    (check-equal? (fpf:divide-terms-general (mkTerms '((1 2) 3)) (mkTerms '((0 3) 2)) + * / - vector)
                  (vector '() (mkTerms '((1 2) 3))))
    (check-equal? (fpf:divide-terms-general (mkTerms '((1 2) 3) '((1 1) 3))
                                            (mkTerms '((0 1) 2) '((1 0) 2))
                                            + * / - vector)
                  (vector (mkTerms '((2 0) -3/2) '((1 1 ) 3/2) '((1 0) 3/2))
                          (mkTerms '((3 0) 3) '((2 0) -3))))
    (check-equal? (fpf:divide-terms (mkTerms '((1 2) 3) '((1 1) 3))
                                    (mkTerms '((0 1) 2) '((1 0) 2))
                                    vector)
                  (vector (mkTerms '((2 0) -3/2) '((1 1 ) 3/2) '((1 0) 3/2))
                          (mkTerms '((3 0) 3) '((2 0) -3))))
    (check-equal? (fpf:divide-terms (mkTerms '((1 2) 3) '((1 1) 3))
                                    (mkTerms '((0 1) 2) '((1 0) 2)))
                  (list (mkTerms '((2 0) -3/2) '((1 1 ) 3/2) '((1 0) 3/2))
                        (mkTerms '((3 0) 3) '((2 0) -3))))

    (check-equal? (fpf:divide 5 3) '(5/3 0))
    (check-equal? (fpf:divide (fpf:make (mkTerms '((1 2) 3))) 3 vector )
                  (vector (fpf:make (mkTerms '((1 2) 1))) 0))
    (check-equal? (fpf:divide 3 (fpf:make (mkTerms '((1 2) 3))))
                  '(0 3))
    (check-equal? (fpf:divide (fpf:make (mkTerms '((1 2) 3) '((1 1) 3)))
                              (fpf:make (mkTerms '((0 1) 2) '((1 0) 2))))
                  (list (fpf:make (mkTerms '((2 0) -3/2) '((1 1 ) 3/2) '((1 0) 3/2)))
                        (fpf:make (mkTerms '((3 0) 3) '((2 0) -3)))))
    (check-exn #px"Bad arguments -- FPF:DIVIDE"
               (λ () (fpf:divide 'wrong (fpf:make (mkTerms '((0 1) 2) '((1 0) 2))))))
    (let ([A (randFPF (random 10) 3)]
          [B (randFPF (random 10) 3)])
      ;; todo add an fpf:zero?
      (unless (and (not (explicit-fpf? B)) (zero? B))
        (check-equal? (fpf:divide A B (λ (p r) (fpf:clean (fpf:+ (fpf:* p B) r)))) A
                      (format "A: ~a\nB: ~a" A B)))))
   (test-case
    "fpf:horner-eval"
    (check-equal? (fpf:horner-eval-general '() '(5 7) + - * expt)
                  0)
    (check-equal? (fpf:horner-eval-general (mkTerms '((0 0) 3)) '(5 7) + - * expt)
                  3)
    (check-equal? (fpf:horner-eval-general (mkTerms '((1 0) 3)) '(5 7) + - * expt)
                  15)
    (check-equal? (fpf:horner-eval-general (mkTerms '((1 0) 3) '((0 1) 2)) '(5 7) + - * expt)
                  29)
    (check-equal? (fpf:horner-eval-general (mkTerms '((1 0) 3) '((0 1) 2)) '(5 7) + - * expt)
                  29)
    (check-equal? (fpf:horner-eval-general (mkTerms '((0) 3) '((5) 2) '((2) 1)) '(4) + - * expt)
                  2067)

    (check-equal? (fpf:horner-eval 3 '(4)) 3)
    (check-equal? (fpf:horner-eval (fpf:make (mkTerms '((0) 3) '((5) 2) '((2) 1))) '(4))
                  2067)
    (check-equal? (fpf:horner-eval (fpf:make (mkTerms '((1 0) 3) '((0 1) 2))) '(5 7))
                  29)
    ;;TODO;; this should have a better error message
    (check-exn #px"map: all lists must have same size"
               (λ () (fpf:horner-eval (fpf:make (mkTerms '((0) 3) '((5) 2) '((2) 1))) '(4 5)))))
   (test-case
    "<->expression"
    (check-equal? (fpf:->expression 3 '((sqrt x) y))
                  3)
    (check-equal? (fpf:->expression (fpf:make (mkTerms '((1 0) 3) '((1 2) 2)))
                                    '((sqrt x) y))
                  '(+ (* 3 (sqrt x)) (* 2 (sqrt x) (expt y 2))))
    (check-exn #px"Bad fpf -- ->EXPRESSION"
               (λ () (fpf:->expression 'wrong '((sqrt x) y))))

    (check-equal? (fpf:expression-> '(+ (* 3 x) (* 2 x (expt y 2))) vector)
                  (vector (fpf:make (mkTerms '((1 2) 2) '((1 0) 3)))
                          '(x y)))
    ;;TODO;; error on unknown (symbolic) operators
    (check-equal? (fpf:expression-> '(+ (* 3 (- x)) (* (negate 2) (square x) (expt y 2))) vector (λ (a b) (string>? (format "~a" a) (format "~a" b))))
                  (vector (fpf:make (mkTerms '((2 2) -2) '((0 1) -3)))
                          '(y x))))
   (test-case
    "$fpf"
    (check-equal? (+$fpf 3 (fpf:make (mkTerms '((0) 3) '((5) 2) '((2) 1))) 3)
                  (fpf:make (mkTerms '((0) 9) '((5) 2) '((2) 1))))
    (check-equal? (-$fpf 3 (fpf:make (mkTerms '((0) 3) '((5) 2) '((2) 1))) 3)
                  (fpf:make (mkTerms '((0) -3) '((5) -2) '((2) -1))))
    (check-equal? (*$fpf 3 (fpf:make (mkTerms '((0) 3) '((5) 2) '((2) 1))) 3)
                  (fpf:make (mkTerms '((5) 18) '((2) 9) '((0) 27)))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))