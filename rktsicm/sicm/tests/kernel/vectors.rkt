#lang racket/base

(require rackunit
         "../../kernel/vectors.rkt"
         "../../kernel/types.rkt"
         (only-in "../../kernel/cstm/arity.rkt" *at-least-zero*)
         (only-in "../../kernel/cstm/express.rkt" expression-of)
         (only-in "../../kernel/generic.rkt" g:zero? g:solve-linear g:solve-linear-left)
         "../helper.rkt"
         )

(provide the-tests)
(define the-tests
  (test-suite
   "kernel/vectors"
   ;; CSTM
   (test-case
    "type"
    (check-equal? (v:type 'any) vector-type-tag)
    (check-equal? (v:type-predicate 'any) vector-quantity?)
    (check-true ((v:type-predicate 'any) (vector 3 4))))
   (test-case
    "v:dimension"
    (check-exn #px"vector-length: contract violation\\\n  expected: vector\\?"
               (λ () (v:dimension 'any)))
    (check-equal? (v:dimension (vector 3 4)) 2))
   (test-case
    "renamed constructors"
    (check-equal? (v:generate 4 (λ (i) i)) (vector 0 1 2 3))
    (check-equal? (vector:generate 4 (λ (i) i)) (vector 0 1 2 3)))
   (test-case
    "cross-product"
    (check-exn #px"Cross product of non-3-dimensional vectors\\?" (λ () (v:cross-product #(0 1 2) #())))
    (check-equal? (v:cross-product #(1 0 0) #(0 1 0)) #(0 0 1))
    (check-equal? (v:cross-product #(0 1 0) #(0 0 1)) #(1 0 0))
    (check-equal? (v:cross-product #(1 0 0) #(0 0 1)) #(0 -1 0))
   (test-case
    "dot-product"
    (check-exn #px"Not vectors -- V:DOT-PRODUCT" (λ () (v:dot-product #() 0)))
    (check-exn #px"Not same dimension -- V:DOT-PRODUCT" (λ () (v:dot-product #() #(0))))
    (check-equal? (v:dot-product #() #()) 0)
    (check-equal? (v:dot-product #(1) #(3)) 3)
    (check-equal? (v:dot-product #(1 2) #(3 9)) 21)
    (check-equal? (v:dot-product #(1+i 2) #(3 9)) 21+3i)))
   ;; MAIN
   (test-case
    "inner-product"
    (check-exn #px"Not vectors -- INNER-PRODUCT" (λ () (v:inner-product #() 0)))
    (check-exn #px"Not same dimension -- INNER-PRODUCT" (λ () (v:inner-product #() #(0))))
    (check-equal? (v:inner-product #() #()) 0)
    (check-equal? (v:inner-product #(1) #(3)) 3)
    (check-equal? (v:inner-product #(1 2) #(3 9)) 21)
    (check-equal? (v:inner-product #(1+i 2) #(3 9)) 21-3i))
   (test-case
    "general-inner-product"
    (define (s+ s1 s2) (bytes->string/latin-1
                        (list->bytes
                         (map (λ (a b) (modulo (+ a b) 256))
                              (bytes->list (string->bytes/latin-1 s1))
                              (bytes->list (string->bytes/latin-1 s2))))))
    (define s* string-append)
    (define GIP-string (general-inner-product s+ s* ""))
    (check-exn #px"vector-length: contract violation\\\n  expected: vector\\?" (λ () (GIP-string #() 0)))
    (check-exn #px"Unequal dimensions -- INNER-PRODUCT" (λ () (GIP-string #() #(0))))
    (check-equal? (GIP-string #() #()) "")
    (check-equal? (GIP-string #("one") #("two")) "onetwo")
    (check-equal? (GIP-string #("one" "two") #("vier" "vijf")) "ãåÔìÒÏØ"))
   (test-case
    "zero"
    (check-true (v:zero? #(0 0 0)))
    (check-false (v:zero? #(0 0 0 1)))
    (check-equal? (v:make-zero 5) #(0 0 0 0 0))
    (check-true (v:zero? (v:make-zero 0)))
    (check-true (v:zero? (v:make-zero 1)))
    (check-equal? (v:zero-like #(a 2 3)) #(0 0 0))
    (check-equal? (v:zero-like #(#(1) 2 3)) #(#(0) 0 0)))
   (test-case
    "elementwise"
    (check-exn #px"assertion failed:" (λ () ((v:elementwise +) #(1 2) #(3))))
    (check-equal? ((v:elementwise +) #(1 2) #(3 4)) #(4 6))
    (check-equal? v:elementwise vector:elementwise))
   (test-case
    "base"
    (check-equal? (build-list 3 (λ (i) (v:make-basis-unit 3 i)))
                  '(#(1 0 0) #(0 1 0) #(0 0 1)))
    (check-equal? (v:basis-unit? (v:make-basis-unit 3 1)) 1)
    (check-equal? (v:basis-unit? (v:make-basis-unit 8 4)) 4)
    (check-false (v:basis-unit? #(0 1 1 0))))
   (test-case
    "literals"
    (check-equal? (literal-vector 'A 3) #(A^0 A^1 A^2)))
   (test-case
    "="
    (check-true  (vector=vector #(1 2) #(1 2)))
    (check-false (vector=vector #(1 2) #(1 3)))
    (check-exn #px"" (λ () (vector=vector #(1 2) #(1 2 3))))
    (check-exn #px"" (λ () (vector=vector #(1 2 3) #(1 2)))))
   (test-case
    "+ / -"
    (check-equal? (vector+vector #(1 2) #(2 1)) #(3 3))
    (check-equal? (vector-vector #(2 1) #(1 2)) #(1 -1))
    (check-equal? (v:negate #(1 2)) #(-1 -2))
    (check-equal? (vector+vector #(2 1) (v:negate #(1 2))) #(1 -1))
    ;; TODO: this should fail -> iterat:vector-forall
    (skip (check-exn #px"TODO" (λ () (vector+vector #(1 2) #(1 2 3))))
          (check-exn #px"TODO" (λ () (vector-vector #(1 2 3) #(1 2))))))
   (test-case
    "* / /"
    (check-equal? ((v:scale 5) #(2 4)) #(10 20))
    (check-equal? (scalar*vector 5 #(2 #(4))) #(10 #(20)))
    (check-equal? (vector*scalar #(2 #(4)) -1) #(-2 #(-4)))
    (check-equal? (vector/scalar #(2 4) 2) #(1 2))
    (check-equal? (vector/scalar #(#(2 4)) 2) #(#(1 2))))
   (test-case
    "² ³"
    (check-equal? (v:square #(1 2)) 5)
    (check-equal? (v:cube #(1 2)) #(5 10)))
   (test-case
    "unit & norm"
    (check-equal? (euclidean-norm #(1 2)) (sqrt 5))
    (check-equal? (euclidean-norm #(+i 2)) (sqrt 3))
    (check-equal? (complex-norm #(+i 2)) (sqrt 5))
    (check-equal? (maxnorm #(1 2)) 2)
    (check-equal? (v:make-unit #(1 2)) (vector (/ 1 (sqrt 5)) (/ 2 (sqrt 5))))
    (check-= (euclidean-norm (v:make-unit #(1 2))) 1 1e-15)
    (check-true (v:unit? (v:make-unit #(3 4))))
    (check-false (v:unit? #(1 2))))
   (test-case
    "conjugate"
    (check-equal? (v:conjugate #(1 +i 1-i)) #(1 -i 1+i)))
   (test-case
    "applicable vectors"
    (define V (vector (λ (x) x) (λ x (car x))))
    (check-exn #px"I don't know the arity of the empty vector" (λ () (v:arity #())))
    (check-equal? (v:arity V) 1)
    (check-equal? (v:arity (vector (λ () 1) (λ (x) x))) #f)
    (check-equal? (v:apply V '(1)) #(1 1)))
   (test-case
    "inexact"
    (check-false (v:inexact? #(#(1) 1)))
    (check-true (v:inexact? #(#(1) 1 2. #(2))))
    (check-true (v:inexact? #(#(1) 1 2 #(2.)))))
   (test-case
    "partial-derivative"
    (check-equal? (v:apply (v:partial-derivative (vector (λ (x y) x)
                                                         (λ (x y) y))
                                                 '(0))
                           '(2 3))
                  #(1 0))
    (check-equal? (v:apply (v:partial-derivative (vector (λ (x y) x)
                                                         (λ (x y) y))
                                                 '(1))
                           '(2 3))
                  #(0 1)))
   (test-case
    "solve"
    (check-equal? (g:solve-linear-left 3 #(3 6)) #(1 2))
    (check-equal? (g:solve-linear 2 #(4 2)) #(2 1)))

   (test-case
    "abstract"
    (check-true (vector-quantity? #(1)))
    (check-true (vector-quantity? (abstract-vector 'A)))
    (check-equal? (av:arity (abstract-vector 'A)) *at-least-zero*)
    (check-true (g:zero? (av:zero-like #(1 2 3)))))
   (test-case
    "abstract-combination"
    (check-equal? (expression-of ((make-vector-combination '+) 'A 'B)) '(+ A B))
    (check-equal? (expression-of ((make-vector-combination '+ #t) 'A 'B)) '(+ B A)))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))