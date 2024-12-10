#lang racket/base

(require rackunit
         "../../kernel/utils.rkt"
         "../helper.rkt"
         )

(provide the-tests)
(define the-tests
  (test-suite
   "kernel/utils"
   (test-case "ORIG:defer-application"
              (check-equal?
               ((((defer-application (lambda (x) (* 3 x))) (lambda (x) (+ x 2)))
                 (lambda (x) (/ x 2))) 3)
               21/2))
   ;; ***************************************************************************
   (test-case
    "do-up / high"
    (check-equal? (accumulate acc (do-up 4 8 (λ (i) (acc i))))
                  '(4 5 6 7))
    (check-equal? (accumulate acc (do-down 9 3 (λ (i) (acc i))))
                  '(9 8 7 6 5 4)))
   (test-case
    "sign"
    (check-equal? (sign 1e-300) 1)
    (check-equal? (sign +inf.0) 1)
    (check-equal? (sign -inf.0) -1)
    (check-equal? (sign -3.8) -1)
    (check-equal? (sign 0.0) 0)
    (check-equal? (sign -0.0) 0)
    (check-equal? (sign +nan.0) 0))
   (test-case
    "pairwise-test"
    (check-true ((make-pairwise-test <) 1 2 3 4))
    (check-false ((make-pairwise-test <) 1 2 3 5 4))
    (check-exn #px"Pred needs 2 args" (λ () ((make-pairwise-test <) 1))))
   (test-case
    "all-equal?"
    (check-true (all-equal? '()))
    (check-true (all-equal? '(a)))
    (check-true (all-equal? '(a a a a a)))
    (check-false (all-equal? '(a a a a b a))))
   (test-case
    "accumulation / inv."
    (check-equal? ((accumulation list '()) 1 2 3 4) '(((1 2) 3) 4))
    (check-equal? ((accumulation + 0) 1 2 3 4) 10)
    (check-equal? ((accumulation + 0) 1) 1)
    (check-equal? ((accumulation + 0)) 0)
    (check-equal? ((inverse-accumulation vector list box '()) 1 2 3 4) '#(1 ((2 3) 4)))
    (check-equal? ((inverse-accumulation - + (λ (a) (* a -1)) 0) 1 2 3 4) -8)
    (check-equal? ((inverse-accumulation - + (λ (a) (* a -1)) 0) 1 2) -1)
    (check-equal? ((inverse-accumulation - + (λ (a) (* a -1)) 0) 1) -1)
    (check-equal? ((inverse-accumulation - + (λ (a) (* a -1)) 0)) 0))
   (test-case
    "shift"
    (check-equal? (left-circular-shift '(1 2 3)) '(2 3 1))
    (check-equal? (left-circular-shift '(1)) '(1))
    (check-equal? (left-circular-shift '()) '())
    (check-equal? (right-circular-shift '(1 2 3)) '(3 1 2))
    (check-equal? (right-circular-shift '(1)) '(1))
    (check-equal? (right-circular-shift '()) '()))
   (test-case
    "compose"
    (check-equal? ((compose vector list box) 1) #((#&1)))
    (check-equal? ((compose-n '()) 'niks) 'niks)
    (check-equal? ((compose-n (list vector)) 'niks) #(niks))
    (check-equal? ((compose-n (list vector list)) 'niks) #((niks)))
    (check-equal? ((compose-n (list vector list box)) 1) #((#&1)))
    (check-equal? ((compose-2 vector list) 'niks) #((niks)))
    (check-equal? ((compose-2 vector (list list box)) 1) #((1) #&1))

    (check-exn #px"compose-bin\\+1: contract violation\n  expected: first procedure that accepts 1 argument\n  given:"
               (λ () (compose-bin (λ (x y) (+ x y)) (λ (x y) (* x y)))))
    (let ([F (compose-bin (λ (x) (- x)) (λ (x y) (* x y)))])
      (check-equal? (procedure-arity F) 2)
      (check-equal? (F 3 6) -18))
    (let ([F (compose-bin (λ (x) (- x)) (λ (x . y) (apply * x y)))])
      (check-equal? (procedure-arity F) (arity-at-least 1))
      (check-equal? (F 2 3 4) -24))
    (let ([F (compose-bin (λ (x) (- x)) (case-lambda [(x) x][(x y z . q) (apply * x y z q)]))])
      (check-equal? (procedure-arity F) (list 1 (arity-at-least 3)))
      (check-equal? (F 2) -2)
      (check-equal? (F 2 1/2 2 1/2 2 1/2) -1))
    
    (check-exn #px"compose-bin\\+n: contract violation\n  expected: first procedure that accepts 2 argument\\(s\\)\n  given:"
               (λ () (compose-bin (λ (x) (- x)) (list (λ (x y) (* x y)) (λ y (apply * y))))))
    (let ([F (compose-bin (λ (x y) (- x y)) (list (λ (x y) (* x y)) (λ _ 3)))])
      (check-equal? (procedure-arity F) 2)
      (check-equal? (F 3 6) 15))
    (let ([F (compose-bin (λ (x y) (- x y)) (list (λ (x y . z) (apply * x y z)) (λ _ 3)))])
      (check-equal? (procedure-arity F) (arity-at-least 2))
      (check-equal? (F 3 1 6) 15))
    (let ([F (compose-bin (λ (x y) (- x y)) (list (λ _ 3) (case-lambda [(x) x][(x y z . q) (apply * x y z q)])))])
      (check-equal? (procedure-arity F) (list 1 (arity-at-least 3)))
      (check-equal? (F 2) 1)
      (check-equal? (F 2 1/2 2 1/2 2 1/2) 2)))
   (test-case
    "identity / constant"
    (let ([G (gensym)]) (check-equal? (identity G) G))
    (let ([G (gensym)]) (check-equal? ((constant G) 1) G))
    (let ([G (gensym)]) (check-true  (any? G 1 2 3)))
    (let ([G (gensym)]) (check-false (none? G 1 2 3))))
   (test-case
    "listfun"
    (check-exn #px"Reduce no elements" (λ () (a-reduce vector '())))
    (check-equal? (a-reduce vector '(1)) 1)
    (check-equal? (a-reduce vector '(1 2)) #(1 2))
    (check-equal? (a-reduce vector '(1 2 3)) #(#(1 2) 3))
    (check-equal? (filter odd? '(1 2 3 4 5 6)) '(1 3 5))
    (check-equal? ((make-map -) '(3 2 1) '(1 2 5)) '(2 0 -4))
    (check-equal? ((bracket + - * /) 1 2 3) '(6 -4 6 1/6))
    (check-equal? ((apply-to-all vector) '(1 2 3)) '(#(1) #(2) #(3))))
   (test-case
    "combine"
    (check-equal? (((unary-combine vector) list) 1 2 3) #((1 2 3)))
    (check-equal? (((binary-combine vector) + -) 1 2 3) #(6 -4))
    (check-equal? (((nary-combine vector) + - * /) 1 2 3) #(6 -4 6 1/6)))
   (test-case
    "iterate"
    (check-exn #px"I don't know how to invert -- ITERATED" (λ () (iterated sqrt -1)))
    (check-equal? ((iterated sqrt 1) 4.0) 2.0)
    (check-equal? ((iterated sqrt 2) 16.0) 2.0)
    (check-equal? ((iterated sqrt 0) 16.0) 16.0)
    (check-equal? ((iterated sqrt 0 (λ (x) (expt x 2))) 16.0) 256.0)
    (check-equal? (iterate-until-stable sqrt (λ (fx x) (= fx x)) 2.0) 1.0))
   (test-case
    "vec/arg fcts"
    (check-equal? ((make-function-of-vector +) #(1 2 3)) 6)
    (check-equal? ((make-function-of-arguments (make-function-of-vector +)) 1 2 3) 6))
   (test-case
    "alphaless"
    (check-exn #px"ALPHALESS\\?: Wrong type argument" (λ () (alphaless? '() 'a)))
    (check-exn #px"ALPHALESS\\?: Wrong type argument" (λ () (alphaless? 'a '())))
    (check-exn #px"ALPHALESS\\?: Wrong type argument" (λ () (alphaless? "a" '())))
    (check-true (alphaless? 'a 'b))
    (check-true (alphaless? 'a "b"))
    (check-true (alphaless? "a" "b"))
    (check-true (alphaless? "a" 'b))
    (check-false (alphaless? 'b 'a))
    (check-false (alphaless? 'b "a"))
    (check-false (alphaless? "b" "a"))
    (check-false (alphaless? "b" 'a)))
   (test-case
    "concat"
    (check-equal? (concatenate-names) the-null-symbol)
    (check-equal? (concatenate-names 'a) 'a)
    (check-equal? (concatenate-names 'a 'b) 'a.b)
    (check-equal? (concatenate-names 'a 'b 'c) 'a.b.c))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))