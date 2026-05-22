#lang racket/base

(require rackunit
         (only-in "../../kernel.rkt" expression) ;; generics need to be loaded
         "../../simplify/sparse.rkt"
         (only-in "../../simplify/fpf.rkt" fpf:make fpf:make-term)
         "../helper.rkt")

(define (mkTerms . a) (map (λ (x) (sparse-term (car x) (cadr x))) a))
(define (randExp [n 5] [m 20]) (build-list n (λ _ (random m))))
(define (randTerm [n 5] [m 20] [t 50])
  (define T (if (number? t)
                (if (exact? t) (λ () (- (random (* 2 t)) t)) (λ () (- (* 2 t (random)) t)))
                t))
  (sparse-term (randExp n m) (T)))
(define (randS [r 5] [n 5] [m 20] [t 50])
  (S:clean (build-list r (λ _ (randTerm n m t)))))
(define (S:clean F)
  (for/fold ([H #hash()]
             #:result (sort (for/list ([(k v) (in-hash H)] #:unless (= v 0)) (sparse-term k v)) sparse-term->))
            ([t (in-list F)])
    (hash-update H (sparse-exponents t) (λ (x) (+ x (sparse-coefficient t))) 0)))

(provide the-tests)
(define the-tests
  (test-suite
   "simplify/sparse"
   (test-case
    "sparse-term"
    (define T (sparse-term '(1 2 3) 4))
    (check-equal? (sparse-exponents T) '(1 2 3))
    (check-equal? (sparse-coefficient T) 4)
    
    (check-true  (sparse-constant-term? (sparse-term (build-list (random 10) (λ _ 0)) 3)))
    (check-false (sparse-constant-term? (sparse-term '(0 0 1 0) 3)))
    
    (check-true  (sparse-univariate? (mkTerms '((1) 3) '((2) 8))))
    (check-true  (sparse-univariate? (mkTerms '((0) 3))))
    (check-false (sparse-univariate? (mkTerms '((0 1) 3))))
    (check-false (sparse-univariate? (mkTerms '(() 3))))
    
    (check-true  (sparse-constant? (mkTerms '((0) 3))))
    (check-true  (sparse-constant? (mkTerms '((0 0 0) 3))))
    (check-false (sparse-constant? (mkTerms '((1) 3))))
    
    (check-true  (sparse-one-term? (sparse-term '(0 0 0) 1)))
    (check-false (sparse-one-term? (sparse-term '(0 0 0) 0)))
    (check-false (sparse-one-term? (sparse-term '(0 0 1) 1)))
    (check-true  (sparse-one? (mkTerms '((0 0 0) 1))))
    (check-false (sparse-one? (mkTerms '((0 0 0) 1) '((0 0 1) 1))))
    (check-false (sparse-one? (mkTerms '((0 0 0) 0))))
    (check-false (sparse-one? (mkTerms '((0 0 1) 1))))
    
    (check-true  (sparse-zero-term? (sparse-term '(0 0 0) 0)))
    (check-false (sparse-zero-term? (sparse-term '(0 0 0) 1)))
    (check-false (sparse-zero-term? (sparse-term '(0 0 1) 0)))
    (check-true  (sparse-zero? (mkTerms)))
    #;(check-true  (sparse-zero? (mkTerms '((0 0 0) 0))))
    (check-false (sparse-zero? (mkTerms '((0 0 0) 0) '((0 0 1) 1))))
    (check-false (sparse-zero? (mkTerms '((0 0 0) 1))))
    (check-false (sparse-zero? (mkTerms '((0 0 1) 0))))

    (check-equal? (sparse-constant-term 3 3) (sparse-term '(0 0 0) 3))
    (check-equal? (sparse-constant-term 0 0) (sparse-term '() 0))

    (check-equal? (sparse-one 3) (mkTerms '((0 0 0) 1)))

    (check-equal? (sparse-identity-term 3 1) (sparse-term '(0 1 0) 1))
    ;;TODO should this be an error?
    (check-equal? (sparse-identity-term 2 4) (sparse-term '(0 0) 1))

    (check-equal? (sparse-linear 4 2 3) (mkTerms '((0 0 1 0) 1) '((0 0 0 0) -3)))
    (check-equal? (sparse-linear 3 1 0) (mkTerms '((0 1 0) 1))))
   (test-case
    ">"
    (check-false (sparse:>exponents? '() '()))
    (check-false (sparse:>exponents? '() '(1)))
    (check-true  (sparse:>exponents? '(1) '()))
    (check-true  (sparse:>exponents? '(2) '(1)))
    (check-false (sparse:>exponents? '(1) '(2)))
    (check-false (sparse:>exponents? '(2) '(2)))
    (check-true  (sparse:>exponents? '(2 1) '(2)))
    (check-false (sparse:>exponents? '(2 1) '(2 1)))
    (check-true  (sparse:>exponents? '(0 2 3) '(1 2)))
    (check-false (sparse:>exponents? '(0 1 1 1) '(1 0 1 1)))
    (check-true  (sparse:>exponents? '(1 1 0 1) '(1 0 1 1)))
    (check-true  (sparse:>exponents? '(1 0 0) '(1)))
    (check-false (sparse:>exponents? '(1) '(1 0 0)))

    (let ([fs1 (randExp (random 10))]
          [fs2 (randExp (random 10))])
      (check-equal? (sparse:>exponents? fs1 fs2)
                    (sparse-term-> (sparse-term fs1 (random 10))
                                   (sparse-term fs2 (random 10))))))
   (test-case
    "sparse-normalize"
    (check-equal? (sparse-normalize (mkTerms '((1 2 3) 4) '((5 6 7) 8)) 1)
                  (mkTerms '((1 2 3) 4) '((5 6 7) 8)))
    ;;TODO 1 is allowed but other numbers are not!?!
    (check-equal? (sparse-normalize (mkTerms '((1 2 3) 4) '((5 6 7) 8)) (sparse-constant-term 3 1))
                  (mkTerms '((1 2 3) 4) '((5 6 7) 8)))
    (check-equal? (sparse-normalize (mkTerms '((1 2 3) 4) '((5 6 7) 8)) (sparse-term '(0 0 0) 3))
                  (mkTerms '((1 2 3) 4/3) '((5 6 7) 8/3)))
    (check-equal? (sparse-normalize (mkTerms '((1 2 3) 4) '((5 6 7) 8)) (sparse-term '(1 0 3) 3))
                  (mkTerms '((0 2 0) 4/3) '((4 6 4) 8/3))))
   (test-case
    "sparse-scale"
    (check-equal? (sparse-scale (mkTerms '((1 2 3) 4) '((5 6 7) 8)) 1)
                  (mkTerms '((1 2 3) 4) '((5 6 7) 8)))
    ;;TODO 1 is allowed but other numbers are not!?!
    (check-equal? (sparse-scale (mkTerms '((1 2 3) 4) '((5 6 7) 8)) (sparse-constant-term 3 1))
                  (mkTerms '((1 2 3) 4) '((5 6 7) 8)))
    (check-equal? (sparse-scale (mkTerms '((1 2 3) 4) '((5 6 7) 8)) (sparse-term '(0 0 0) 3))
                  (mkTerms '((1 2 3) 12) '((5 6 7) 24)))
    (check-equal? (sparse-scale (mkTerms '((1 2 3) 4) '((5 6 7) 8)) (sparse-term '(1 0 3) 3))
                  (mkTerms '((2 2 6) 12) '((6 6 10) 24))))
   (test-case
    "sparse-negate-term"
    (check-equal? (sparse-negate-term (sparse-term '(1 2 3) 4)) (sparse-term '(1 2 3) -4)))
   (test-case
    "sparse-add"
    (check-equal? (sparse-add '() '()) '())
    (check-equal? (sparse-add '() (mkTerms '((1 2 3) 4) '((5 6 7) 8))) (mkTerms '((1 2 3) 4) '((5 6 7) 8)))
    (check-equal? (sparse-add (mkTerms '((1 2 3) 4) '((5 6 7) 8)) '()) (mkTerms '((1 2 3) 4) '((5 6 7) 8)))
    (check-equal? (sparse-add (mkTerms '((5 6 7) 8) '((1 2 3) 4))
                              (mkTerms '((5 6 7) 8) '((1 2 3) 4)))
                  (mkTerms '((5 6 7) 16) '((1 2 3) 8)))
    ;;TODO; make sure sparse polys are ordered
    (check-equal? (sparse-add (mkTerms '((5 6 7) 8) '((1 2 3) 4))
                              (mkTerms '((1 2 3) 4) '((5 6 7) 8)))
                  (mkTerms '((5 6 7) 8) '((1 2 3) 8) '((5 6 7) 8)))
    (check-equal? (sparse-add (mkTerms '((1 2 3) 4) '((5 6 7) 8))
                              (mkTerms '((5 6 7) 8) '((1 2 3) 4)))
                  (mkTerms '((5 6 7) 8) '((1 2 3) 8) '((5 6 7) 8)))
    (check-equal? (sparse-add (mkTerms '((5 6 7) 8) '((1 2 3) 4))
                              (mkTerms '((1 2 3) -4) '((5 6 7) 8)))
                  (mkTerms '((5 6 7) 8) '((5 6 7) 8))))
   (test-case
    "sparse-multiply"
    (check-equal? (sparse-multiply-term (sparse-term '(1 2) 3) '()) '())
    (check-equal? (sparse-multiply-term (sparse-term '(1 2) 3) (mkTerms '((1 2) 3) '((3 4) 2)))
                  (mkTerms '((2 4) 9) '((4 6) 6)))

    (check-equal? (sparse-multiply '() (mkTerms '((1 2) 3) '((3 4) 2))) '())
    (check-equal? (sparse-multiply (mkTerms '((1 2) 3)) (mkTerms '((1 2) 3) '((3 4) 2)))
                  (mkTerms '((2 4) 9) '((4 6) 6)))
    (check-equal? (sparse-multiply (mkTerms '((1 2) 3) '((1 1) 1)) (mkTerms '((1 2) 3) '((3 4) 2)))
                  (mkTerms '((2 4) 9) '((4 6) 6) '((2 3) 3) '((4 5) 2))))
   (test-case
    "sparse-abs"
    (check-equal? (sparse-abs '()) '())
    (check-equal? (sparse-abs (mkTerms '((1 1) 3) '((1 0) -1)))
                  (mkTerms '((1 1) 3) '((1 0) -1)))
    (check-equal? (sparse-abs (mkTerms '((1 1) -3) '((1 0) -1)))
                  (mkTerms '((1 1) 3) '((1 0) 1))))

   (test-case
    "sparse-divide"
    ;;TODO;; is this not just duplication of fpf:divide-term-general (same as multiply and add)
    (check-equal? (sparse-divide '() (mkTerms '((0 1) 2)) vector)
                  (vector '() '()))
    (check-equal? (sparse-divide (mkTerms '((1 2) 3)) (mkTerms '((0 1) 2)) vector)
                  (vector (mkTerms '((1 1) 3/2)) '()))
    (check-equal? (sparse-divide (mkTerms '((1 2) 3)) (mkTerms '((0 2) 2)) vector)
                  (vector (mkTerms '((1 0) 3/2)) '()))
    (check-equal? (sparse-divide (mkTerms '((1 2) 3)) (mkTerms '((0 3) 2)) vector)
                  (vector '() (mkTerms '((1 2) 3))))
    (check-equal? (sparse-divide (mkTerms '((1 2) 3) '((1 1) 3))
                                 (mkTerms '((0 1) 2) '((1 0) 2))
                                 vector)
                  (vector (mkTerms '((2 0) -3/2) '((1 1 ) 3/2) '((1 0) 3/2))
                          (mkTerms '((3 0) 3) '((2 0) -3))))

    (let ([A (randS (random 10) 3)]
          [B (randS (random 10) 3)])
      ;; todo add an fpf:zero?
      (unless (sparse-zero? B)
        (check-equal? (sparse-divide A B (λ (p r) (S:clean (sparse-add (sparse-multiply p B) r)))) A
                      (format "A: ~a\nB: ~a" A B))))

    (check-true  (sparse-divisible? (mkTerms '((1 2) 3)) (mkTerms '((0 1) 2))))
    (check-false (sparse-divisible? (mkTerms '((1 2) 3)) (mkTerms '((0 3) 2)))))
   (test-case
    "fpf->"
    (check-equal? (fpf:->sparse (fpf:make (list (fpf:make-term '(1 2 3) 3) (fpf:make-term '(1 2 2) 1))))
                  (mkTerms '((1 2 3) 3) '((1 2 2) 1))))
   (test-case
    "sparse-evaluate"
    ;;TODO;; why not make this work for fpf too? (in scmutils it also doesn't work)
    (check-equal? (sparse-evaluate '() '(5 7))
                  0)
    (check-equal? (sparse-evaluate (mkTerms '((0 0) 3)) '(5 7))
                  3)
    (check-equal? (sparse-evaluate (mkTerms '((1 0) 3)) '(5 7))
                  15)
    (check-equal? (sparse-evaluate (mkTerms '((1 0) 3) '((0 1) 2)) '(5 7))
                  29)
    (check-equal? (sparse-evaluate (mkTerms '((1 0) 3) '((0 1) 2)) '(5 7))
                  29)
    (check-equal? (sparse-evaluate (mkTerms '((0) 3) '((5) 2) '((2) 1)) '(4))
                  2067)
    (check-equal? (expression (sparse-evaluate (mkTerms '((1 0) 3) '((0 1) 2)) '(x y)))
                  '(+ (* 3 x) (* 2 y)))
    (check-exn #px"assertion failed: \\(fix:= \\(length x\\)"
               (λ () (sparse-evaluate (mkTerms '((0 0) 3)) '(5 7 7))))
    (check-exn #px"assertion failed: \\(fix:= \\(length x\\)"
               (λ () (sparse-evaluate (mkTerms '((0 0) 3)) '(5))))

    (check-equal? (sparse-evaluate> '() '(5 7))
                  '())
    (check-equal? (sparse-evaluate> (mkTerms '((0 0) 3)) '(5 7))
                  (mkTerms '(() 3)))
    (check-equal? (sparse-evaluate> (mkTerms '((1 0) 3)) '(5 7))
                  (mkTerms '(() 15)))
    (check-equal? (sparse-evaluate> (mkTerms '((1 0) 3) '((0 1) 2)) '(5 7))
                  (mkTerms '(() 29)))
    (check-equal? (sparse-evaluate> (mkTerms '((1 0) 3) '((0 1) 2)) '(5 7))
                  (mkTerms '(() 29)))
    (check-equal? (sparse-evaluate> (mkTerms '((0) 3) '((5) 2) '((2) 1)) '(4))
                  (mkTerms '(() 2067)))
    (check-exn #px"take: contract violatio" ;;TODO;; better error
               (λ () (sparse-evaluate> (mkTerms '((0 0) 3)) '(5 7 7))))
    (check-equal? (sparse-evaluate> (mkTerms '((1 2) 3)  '((1 0) 3) '((0 0) 3)) '(5))
                  (mkTerms  '((1) 78) '((0) 3)))
    (check-equal? (sparse-evaluate> (mkTerms '((2 1) 3)  '((0 1) 3) '((0 0) 3)) '(5))
                  (mkTerms  '((2) 15) '((0) 18)))

    (check-equal? (sparse-evaluate< '() '(5 7))
                  '())
    (check-equal? (sparse-evaluate< (mkTerms '((0 0) 3)) '(5 7))
                  (mkTerms '(() 3)))
    (check-equal? (sparse-evaluate< (mkTerms '((1 0) 3)) '(5 7))
                  (mkTerms '(() 15)))
    (check-equal? (sparse-evaluate< (mkTerms '((1 0) 3) '((0 1) 2)) '(5 7))
                  (mkTerms '(() 29)))
    (check-equal? (sparse-evaluate< (mkTerms '((1 0) 3) '((0 1) 2)) '(5 7))
                  (mkTerms '(() 29)))
    (check-equal? (sparse-evaluate< (mkTerms '((0) 3) '((5) 2) '((2) 1)) '(4))
                  (mkTerms '(() 2067)))
    (check-exn #px"list-tail: index too large for list" ;;TODO;; better error
               (λ () (sparse-evaluate< (mkTerms '((0 0) 3)) '(5 7 7))))
    (check-equal? (sparse-evaluate< (mkTerms '((1 2) 3)  '((1 0) 3) '((0 0) 3)) '(5))
                  (mkTerms  '((2) 15) '((0) 18)))
    (check-equal? (sparse-evaluate< (mkTerms '((2 1) 3)  '((0 1) 3) '((0 0) 3)) '(5))
                  (mkTerms  '((1) 78) '((0) 3))))

   (test-case
    "sparse-combine/merge"
    (check-equal? (sparse-merge-adjacent-terms (mkTerms)) '())
    (check-equal? (sparse-merge-adjacent-terms (mkTerms '((1 2) 3))) (mkTerms '((1 2) 3)))
    (check-equal? (sparse-merge-adjacent-terms (mkTerms '((1 2) 0))) '())
    (check-equal? (sparse-merge-adjacent-terms (mkTerms '((1 2) 1) '((1 2) 2)))
                  (mkTerms '((1 2) 3)))
    (check-equal? (sparse-merge-adjacent-terms (mkTerms '((1 2) 1) '((1 2) -1) '((2 2) 3)))
                  (mkTerms '((2 2) 3)))
    (check-equal? (sparse-merge-adjacent-terms (mkTerms '((6 3) 1) '((1 2) 1) '((1 2) -1) '((2 2) 3)))
                  (mkTerms '((6 3) 1) '((2 2) 3)))
    (check-equal? (sparse-merge-adjacent-terms (mkTerms '((1 2) 1) '((6 3) 1) '((1 2) -1) '((2 2) 3)))
                  (mkTerms '((1 2) 1) '((6 3) 1) '((1 2) -1) '((2 2) 3)))
    
    (check-equal? (sparse-combine-like-terms (mkTerms '((1 2) 1) '((6 3) 1) '((1 2) -1) '((2 2) 3)))
                  (mkTerms '((6 3) 1) '((2 2) 3))))
   
   ;**************************************************************************************************
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