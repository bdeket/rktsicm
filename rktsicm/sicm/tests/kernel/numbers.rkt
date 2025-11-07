#lang racket/base

(require rackunit
         "../../kernel/numbers.rkt"
         "../../kernel/types.rkt"
         "../../kernel/cstm/arity.rkt"
         (only-in "../../kernel/generic.rkt" g:apply g:+ g:- g:*) ;; side-effect: numbers:assign-operations
         (only-in "../../kernel/express.rkt" expression)
         "../helper.rkt"
         )

(provide the-tests)
(define (tst f1 f2 [R (* (random) 800)] #:rel [rel #f])
      (if rel
          (check-= (f1 R) (f2 R) (* rel (f2 R)) (format "~a(~a)" f1 R))
          (check-equal? (f1 R) (f2 R) (format "~a(~a)" f1 R))))
(define the-tests
  (test-suite
   "kernel/numbers"
   (test-case
    "type"
    (check-equal? (n:type 'any) number-type-tag)
    (check-equal? (n:type-predicate 'any) numerical-quantity?)
    (check-true ((n:type-predicate 'any) 3)))
   (test-case
    "application"
    (check-equal? (n:arity 3) *at-least-zero*)
    (parameterize ([*numbers-are-constant-functions* #f])
      (check-exn #px"Application of a number not allowed in" (λ () (n:self 3 4))))
    (parameterize ([*numbers-are-constant-functions* #t])
      (check-equal? (n:self 3 4) 3)
      (check-equal? (g:apply 3 '(4 5 6)) 3)))
   (test-case
    "derivative"
    (check-equal? (n:deriv 'any 'and 'everything) 0))
   (test-case
    "standard functions"
    (check-equal? (n:zero-like 2) 0)
    (check-equal? (n:zero-like 2.) 0.)
    (check-equal? (n:one-like 2) 1)
    (check-equal? (n:one-like 2.) 1.)
    ;; TODO: is the implementation really better
    (check-equal? (n:sqrt 9/36) (sqrt 9/36))
    (tst n:sqrt sqrt)
    (check-equal? (n:revdivide 3 4) (/ 4 3)))
   (test-case
    "literal/abstract-numbers"
    (check-equal? (expression (literal-number 'f)) 'f)
    (check-true  (literal-number?  (literal-number 'f)))
    (check-true  (abstract-number? (literal-number 'f)))
    (check-false (literal-number?  'f))
    (check-true  (abstract-number? 'f))
    (check-true  (literal-number?  (g:+ 1 'a 3)))
    (check-true  (abstract-number? (g:+ 1 'a 3)))
    (check-false (abstract-number? '(+ 4 a)))
    (check-equal? (expression ((make-numerical-combination '+) 3 4)) 7)
    (check-equal? (expression ((make-numerical-combination '+) 3 (literal-number 'b) 4)) '(+ 7 b))
    (check-equal? (expression ((make-numerical-combination '+ #t) 'a (literal-number 'b))) '(+ b a)))
   (test-case
    "literal/numerical-quantity?"
    (check-true (numerical-quantity? 1))
    (check-true (numerical-quantity? 'a))
    (check-false (numerical-quantity? #()))
    (add-to-numerical-quantity? (λ (x) (and (vector? x) (= (vector-length x) 0))))
    (check-true (numerical-quantity? #())))
   (test-case
    "0/1 for literal-numbers"
    (check-equal? (an:zero-like (literal-number 'f)) 0)
    (check-equal? (an:one-like  'f) 1))
   (test-case
    "tests for literal-numbers"
    ;; these an:~ checks are _NOT_ installed as generic (see comment in src) because of
    ;; the heavy burden on simplify
    (check-false (an:= (g:+ 4 'a) (g:+ 1 'a)))
    (check-true  (an:zero? 0))
    (check-false (an:zero? (literal-number 'a)))
    (check-true  (an:one?  1))
    (check-false (an:one?  (g:- (g:* 2 'a) 'a 'a)))

    (check-true  (abn:= (g:+ 4 'a) (g:+ 1 'a 3)))
    (check-true  (abn:= (literal-number 5) (literal-number 5.)))
    (check-false (abn:= (literal-number '(+ a 1)) (literal-number '(+ 1 a))))
    (check-true  (abn:zero? (literal-number 0)))
    (check-false (abn:zero? (literal-number '(+ 0))))
    (check-true  (abn:one?  (literal-number 1)))
    (check-false (abn:one?  (literal-number '(+ 1)))))
   (test-case
    "known-reals"
    (define b (literal-number 'b))
    (check-false (known-real? 'a))
    (check-false (known-real? b))
    (with-known-reals (list 'a b)
                      (λ ()
                        (check-true (known-real? 'a))
                        (check-true (known-real? b))))
    (check-false (known-real? 'a))
    (declare-known-reals 'a b)
    (check-true (known-real? 'a))
    (check-true (known-real? b))
    (check-true (known-real? (vector 'a b)))
    (local-require (only-in "../../kernel/matrices.rkt" matrix-by-rows)
                   (only-in "../../kernel/diff.rkt" make-differential-quantity make-differential-term))
    (check-true (known-real? (matrix-by-rows '(a) (list b))))
    (check-false (known-real? (matrix-by-rows '(a) (list b) '(c))))
    (check-true (known-real? (make-differential-quantity (list (make-differential-term '() 'a)))))
    (declare-unknown-reals b)
    (check-true (known-real? 'a))
    (check-false (known-real? b)))
   (test-case
    "using simplify"
    (check-false (an:zero? (g:- (g:* 2 'a) 'a 'a)))
    (check-false (an:one?  (g:+ 1 (g:* 2 'a) (g:* -2 'a))))
    (check-false (an:= (g:* 2 'a) (g:+ 'a 'a)))
    (local-require "../../simplify/default.rkt")
    (simplify:assign-operations)
    (check-true (an:zero? (literal-number 0)))
    (check-true (an:one? (literal-number 1)))
    (check-true (an:zero? (g:- (g:* 2 'a) 'a 'a)))
    (check-true (an:one?  (g:+ 1 (g:* 2 'a) (g:* -2 'a))))
    (check-true (an:= (g:* 2 'a) (g:+ 'a 'a)))
    (check-true (known-real? 'a))
    (check-true (known-real? (g:+ (g:* 2 'a) (g:* -1 'a)))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))