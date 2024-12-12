#lang racket/base

(require rackunit
         "../../kernel/structs.rkt"
         "../../kernel/types.rkt"
         "../../kernel/matrices.rkt"
         "../../kernel/express.rkt"
         "../helper.rkt"
         )

(provide the-tests)

(define the-tests
  (test-suite
   "kernel/structs"
   ;; *** cstm **********************************************************************
   (test-case
    "type - construction"
    (check-equal? (map s:type (list (up 4) (down 5) (abstract-up 'f) (abstract-down 'g)))
                  (list up-type-tag down-type-tag up-type-tag down-type-tag))
    (check-exn #px"Bad structure -- S:TYPE" (λ () (s:type 'any)))
    
    (check-true ((sc:type-predicate (up 4)) (up 4)))
    (check-true ((sc:type-predicate (up 4)) (abstract-up 'f)))
    (check-true ((sc:type-predicate (up 4)) (literal-up 'f 2)))
    (check-false ((sc:type-predicate (up 4)) (down 3)))
    (check-true ((sr:type-predicate (down 4)) (down 4)))
    (check-true ((sr:type-predicate (down 4)) (abstract-down 'f)))
    (check-true ((sr:type-predicate (down 4)) (literal-down 'f 1)))
    (check-false ((sr:type-predicate (down 4)) (up 3)))
    (check-true ((sc:type-predicate (up 4)) (vector->up #(4 5 6))))
    (check-true ((sr:type-predicate (down 4)) (vector->down #(4 5 6))))
    (check-true ((sc:type-predicate (up 4)) (s:structure 'up #(4 5 6))))
    (check-true ((sc:type-predicate (up 4)) (s:structure 'contravariant #(4 5 6))))
    (check-true ((sr:type-predicate (down 4)) (s:structure 'covariant #(4 5 6))))
    (check-exn #px"Bad up/down spec -- S:STRUCTURE" (λ () (s:structure 'any #(4 5 6))))
    
    (check-equal? (s:opposite (up 3 2 1)) 'down)
    (check-equal? (s:opposite (down 1 2 3)) 'up)
    (check-exn #px"Bad structure -- S:OPPOSITE" (λ () (s:opposite 'any)))

    (check-equal? (s:same (up 3 2 1)) 'up)
    (check-equal? (s:same (down 1 2 3)) 'down)
    (check-exn #px"Bad structure -- S:SAME" (λ () (s:same 'any))))
   (test-case
    "literal"
    (check-true ((sc:type-predicate (up 4)) (literal-up 'f 4)))
    (check-equal? (s:length (literal-up 'f 4)) 4)
    (check-true ((sr:type-predicate (down 4)) (literal-down 'f 4)))
    (check-equal? (s:length (literal-down 'f 3)) 3))
   (test-case
    "genereate"
    (check-equal? (s:generate 4 'up values) (up 0 1 2 3))
    (check-equal? (s:generate 3 'down add1) (down 1 2 3)))
   (test-case
    "length / dim / ref / select"
    (check-equal? (s:length (up 1 2 3 (up 4 5))) 4)
    (check-equal? (s:length (down 1 2 3)) 3)
    (check-equal? (s:length 'any) 1)
    (check-equal? (s:dimension (up 1 2 3 (up 4 5))) 5)
    (check-equal? (s:dimension 'any) 1)
    (check-equal? (s:ref (up 0 1 2) 2) 2)
    (check-equal? (s:ref (down 0 1 2 3) 1) 1)
    (check-equal? (s:ref 'any 0) 'any)
    (check-exn #px"Bad structure -- S:REF" (λ () (s:ref 'any 2)))
    (check-equal? ((s:select 0 1 0) (down (up 1 (up 3)))) 3))
   (test-case
    "subst"
    (check-equal? (s:with-substituted-coord (up 0 1 2 3 4) 1 'a) (up 0 'a 2 3 4))
    (check-equal? (s:with-substituted-coord (down 0 1 2 3 4) 1 'a) (down 0 'a 2 3 4))
    (check-equal? (s:with-substituted-coord 'any 0 'a) 'a)
    (check-exn #px"Bad structure -- S:WITH-SUBSTITUTED-COORD"
               (λ () (s:with-substituted-coord 'any 1 'a)))
    (check-equal? (s:subst (up (down (up 0 1 2) (up 10 11 12)) (down (up 100 101 102) (up 110 111 112)))
                           'a 0 0 1)
                  (up (down (up 0 'a 2) (up 10 11 12)) (down (up 100 101 102) (up 110 111 112))))
    (check-equal? (s:subst-internal (up (down (up (down 0)))) 'a (list 0 0 0))
                  (up (down (up 'a)))))
   (test-case
    "forall"
    (check-equal? (accumulate acc (s:forall acc (up 1 2 (down 3)))) (list 1 2 (down 3)))
    (check-equal? (s:forall (λ (v) (if (= v 2) #f #t)) (up 1 2 (down 3))) #f)
    (check-equal? (s:forall (λ (v) (if (eq? v 7) #f #t)) (up 1 2 (down 3))) #t))
   (test-case
    "map-chain"
    (check-equal? (s:map-chain (λ (v c) (cons c v)) (up 1 2 (down 3 (up 8))))
                  (up '((0) . 1) '((1) . 2) (down '((2 0) . 3) (up '((2 1 0) . 8))))))
   (test-case
    "fringe / ultra(un)flatten"
    (check-equal? (s:fringe (up (down (up 0 1 2) 7 (up 10 11 12)) (down (up 100 101 102) 8 (up 110 111 112))))
                  '(112 111 110 8 102 101 100 12 11 10 7 2 1 0))
    (check-equal? (ultra-flatten (up (down (up 0 1 2) 7 (up 10 11 12)) (down (up 100 101 102) 8 (up 110 111 112))))
                  '(0 1 2 7 10 11 12 100 101 102 8 110 111 112))
    (check-equal? (ultra-flatten 'any)
                  '(any))
    (check-equal? (ultra-unflatten (up 1 (up 2 (down 3) (up 4)) (down 5)) '(6 7 8 9 10))
                  (up 6 (up 7 (down 8) (up 9)) (down 10)))
    ;; !!!
    (check-equal? (ultra-unflatten (up 1 (up 2 (down 3) (up 4)) (down 5)) '(6 7 8 9 10 11 12))
                  (up 6 (up 7 (down 8) (up 9)) (down 10))))
   (test-case
    "foreach"
    (check-equal? (accumulate acc (s:foreach acc (up (down (up 0 1 2) 7 (up 10 11 12)) (down (up 100 101 102) 8 (up 110 111 112)))))
                  '(0 1 2 7 10 11 12 100 101 102 8 110 111 112)))
   (test-case
    "mappers"
    (check-equal? (s:map/l cons (list (up 1 2 3 (down 4)) (up 'a 'b 'c (down 'd))))
                  (up '(1 . a) '(2 . b) '(3 . c) (cons (down 4) (down 'd))))
    (check-equal? (s:map/l cons (list '(1 2 3) '(a b c)))
                  (cons '(1 2 3) '(a b c)))
    (check-exn #px"valid range" (λ () (s:map cons (up 1 2 3 (down 4)) (up 'a 'b 'c))))
    (check-equal? (s:map/r/l cons (list (up 1 2 3 (down 4)) (up 'a 'b 'c (down 'd))))
                  (up '(1 . a) '(2 . b) '(3 . c) (down '(4 . d))))
    (check-equal? (s:map/r cons (up 1 2 3 (down 4)) (up 'a 'b 'c (down 'd)))
                   (up '(1 . a) '(2 . b) '(3 . c) (down '(4 . d))))
    (skip  ;; !!! this should be errors in my opinion
     (check-equal? (s:map/r cons (up 1 2 3 (down 4)) (up 'a 'b 'c (up 'd)))
                   (up '(1 . a) '(2 . b) '(3 . c) (down '(4 . d))))
     (check-equal? (s:map/r cons (up 1 2 3 (down 4)) (up 'a 'b 'c (up 'd 'e)))
                   (up '(1 . a) '(2 . b) '(3 . c) (down '(4 . d)))))
    (check-equal? ((s:elementwise up) (up 1 2 3 (down 4)) (up 'a 'b 'c (up 'd)))
                  (up (up 1 'a) (up 2 'b) (up 3 'c) (up (down 4) (up 'd)))))
   (test-case
    "rexists"
    (define (Odd? n) (and (number? n) (odd? n)))
    (check-true  (rexists Odd? 1))
    (check-false (rexists Odd? 'any))

    (check-true  (rexists Odd? #(2 4 #(6 8) 9)))
    (check-false (rexists Odd? #(2 4 #(6 8) 10)))

    (check-true  (rexists Odd? (up 2 'b (down 6 8) 9)))
    (check-false (rexists Odd? (down 2 'a (up 6 #(8)) 10)))
    (check-true  (rexists Odd? (up 2 'b (down 6 8) 9)))
    (check-true  (rexists Odd? (up 2 'b (down 6 5) 9)))

    (check-true  (rexists Odd? (matrix-by-rows '(2 4 6) '(1 4 8))))
    ;; !!! better example?
    (check-false (rexists Odd? (abstract-down 'g)))
    (check-false (rexists Odd? '(*abstract-down* . #hash())))
    (check-true  (rexists Odd? (up (list 2 4 9 6))))
    (check-false (rexists Odd? (up (cons 2 4)))))
   (test-case
    "conversion"
    (check-equal? (up->vector (up 1 2 3)) #(1 2 3))
    (check-equal? (down->vector (down 1 2 3)) #(1 2 3))
    
    (check-equal? (s:->vector (up 1 2 3)) #(1 2 3))
    (check-equal? (s:->vector (down 3 2 1)) #(3 2 1))
    (check-exn #px"Bad structure -- S:->VECTOR" (λ () (s:->vector 'any)))

    (check-equal? (vector->up #(4 5 6)) (up 4 5 6))
    (check-equal? (vector->down #(4 5 6)) (down 4 5 6))

    (check-equal? (list->up-structure '(4 5 6)) (up 4 5 6))
    (check-equal? (up-structure->list (up 4 5 6)) '(4 5 6))

    (check-equal? (matrix->structure (matrix-by-rows '(1 2 3) '(4 5 6)))
                  (down (up 1 4) (up 2 5) (up 3 6)))
    (check-equal? (matrix->structure (up 1 2 3)) (up 1 2 3))
    (check-equal? (matrix->structure (down 1 2 3)) (down 1 2 3))
    ;; !!!
    (check-equal? (matrix->structure 'any) 'any))
   ;; *** main **********************************************************************
   (test-case
    "simple forms"
    (check-equal? (s:arity (up 1 2 3)) (arity-at-least 0))
    (check-equal? (s:arity (down (λ (x) 1) (λ _ 2))) 1)
    (check-true  (s:inexact? (up 1 2. 3)))
    (check-false (s:inexact? (down 1 2 3)))
    (check-true  (s:zero? (up 0 0 (up 0))))
    (check-false (s:zero? (down 0 0. (up -0.))))

    (check-equal? (s:zero-like (up 1 2 (up 3) (down 4 5)))
                  (up 0 0 (up 0) (down 0 0)))
    (check-equal? (s:negate (up 1 2 (up 3) (down 4 -5)))
                  (up -1 -2 (up -3) (down -4 5)))
    (check-equal? (s:magnitude (up 1 1+i)) (sqrt 3))
    (check-equal? (s:magnitude (down 3 4)) 5)
    (check-equal? (s:abs (up 1 +i)) 0)
    (check-equal? (s:abs (down 3 4)) 5)
    (check-equal? (s:conjugate (up 1 +i)) (up 1 -i))
    (check-equal? (s:conjugate (down 1 3-i)) (down 1 3+i)))
   (test-case
    " = "
    (check-true  (structure=structure (up 1 2 (down 3)) (up 1 2 (down 3))))
    (check-false (structure=structure (up 1 2 (down 3)) (up 1 2 (up 3))))
    (check-false (structure=structure (up 1 2 (down 3)) (up 1 2 (down 4)))))
   (test-case
    "structure+structure"
    (check-equal? (structure+structure (up 1 2 (down 3)) (up 1 2 (down 3)))
                  (up 2 4 (down 6)))
    (check-exn #px"assertion failed: \\(eq\\? up/down"
               (λ ()(structure+structure (up 1 2 (down 3)) (up 1 2 (up 3)))))
    (check-exn #px"assertion failed: \\(fix:= \\(s:length"
               (λ ()(structure+structure (up 1 2 (down 3)) (up 1 2 (down 3) 4))))
    (check-equal? (structure-structure (up 1 2 (down 3)) (up 1 2 (down 3)))
                  (up 0 0 (down 0)))
    (check-exn #px"assertion failed: \\(eq\\? up/down"
               (λ ()(structure-structure (up 1 2 (down 3)) (up 1 2 (up 3)))))
    (check-exn #px"assertion failed: \\(fix:= \\(s:length"
               (λ ()(structure-structure (up 1 2 (down 3)) (up 1 2 (down 3) 4)))))
   (test-case
    " multiply "
    (parameterize ([*allowing-incompatible-multiplication* (= (random 1) 1)])
      (check-equal? (s:multiply (up 1 2) (down 3 4)) 11)
      (check-equal? (s:multiply (down 5 1/5) (up 1/5 5)) 2)
      (check-equal? (s:multiply (up (down 3 4) (up 2 8)) (down (up 1 2) (down 9 1))) 37)
      (check-exn #px"Incompatible multiplication" (λ () (s:multiply (up 1 2) (down 3 4 5))))
      (check-exn #px"Incompatible multiplication" (λ () (s:multiply (up 1 (up 2)) (down 3 (up 4)))))

      (check-equal? (s:multiply (up 1 2) (up (down 3 4) (down 2 9))) (up 11 20))
      (check-equal? (s:multiply (down 1 2) (down (up 3 4) (up 2 9))) (down 11 20)))
    (parameterize ([*allowing-incompatible-multiplication* #f])
      (check-exn #px"Incompatible multiplication" (λ () (s:multiply (up 1 2) (up 3 4))))
      (check-exn #px"Incompatible multiplication" (λ () (s:multiply (down 1 2) (down (down 3 4) (up 2 9)))))
      (check-exn #px"Incompatible multiplication" (λ () (s:multiply (matrix-by-rows '(2)) (down 1 2)))))
    (parameterize ([*allowing-incompatible-multiplication* #t])
      (check-equal? (s:multiply (up 1 2) (up 3 4)) (up (up 3 6) (up 4 8)))
      (check-equal? (s:multiply (down 1 2) (down (down 3 4) (up 2 9)))
                                     (down (down (down 3 6) (down 4 8)) 20))
      (check-equal? (s:multiply (matrix-by-rows '(2)) (down 1 2))
                    (down (matrix-by-rows '(2)) (matrix-by-rows '(4))))))
   (test-case
    "outer-prod"
    (check-equal? (s:outer-product (up 1 2) (up 3 4)) (up (up 3 6) (up 4 8)))
    (check-equal? (s:outer-product (up 1 2) (down 3 4)) (down (up 3 6) (up 4 8)))
    (check-equal? (s:outer-product (up 1 (up 2)) (down 3 (down 4)))
                  (down (up 3 (up 6)) (down (up 4 (up 8)))))
    (check-equal? (s:outer-product (up 1 (up 2)) (down 3 (up 4)))
                  (down (up 3 (up 6)) (up (up 4 (up 8))))))
   (test-case
    "structure:expt"
    (check-equal? (structure:expt (up 1 2) 1) (up 1 2))
    (check-equal? (structure:expt (up 1 2) 2) (up (up 1 2) (up 2 4)))
    (check-equal? (structure:expt (up 1 2) 3) (up (up (up 1 2) (up 2 4)) (up (up 2 4) (up 4 8))))
    (check-exn #px"Cannot:  \\(expt"(λ () (structure:expt (up 1 2) 0)))
    (check-exn #px"Cannot:  \\(expt"(λ () (structure:expt (up 1 2) -1))))
   (test-case
    "scalar"
    (check-equal? (structure*scalar (up 1 (up 2)) 3) (up 3 (up 6)))
    (check-equal? (scalar*structure 3 (up 1 (up 2))) (up 3 (up 6)))
    (check-equal? (structure/scalar (up 1 (up 2)) 3) (up 1/3 (up 2/3))))
   (test-case
    "square / dot-prod"
    ;; // flat sum of square of all elements ... != expt _ 2 !!
    (check-equal? (s:square (up)) 0)
    (check-equal? (s:square (up 2)) 4)
    (check-equal? (s:square (down 3 4)) 25)
    (check-equal? (s:square (down 3 (up 2 2 2 2))) 25)
    (check-equal? (s:square (down 3 (down 4))) 25)
    (check-equal? (s:dot-product (up 2) (up 2)) 4)
    (check-equal? (s:dot-product (down 2 (up 9)) (down 2 (up 1))) 13)
    (check-exn #px"Incompatible structures -- S:DOT-PRODUCT"
               (λ () (s:dot-product (up 2 (up 9)) (down 2 (up 1)))))
    (check-exn #px"Incompatible structures -- S:DOT-PRODUCT"
               (λ () (s:dot-product (up 2 3) (up 2 3 4))))
    (skip ;; !!! I think this should fail
     (check-exn #px"Incompatible structures -- S:DOT-PRODUCT"
               (λ () (s:dot-product (up 2 (up 9)) (up 2 (down 1)))))))
   (test-case
    "deriv"
    (check-equal? ((s:ref (s:partial-derivative (up (λ (x) x)) '()) 0) 't) 1)
    (check-equal? ((s:ref (s:partial-derivative (down (λ (x) 3)) '()) 0) 't) 0))
   (test-case
    "apply"
    (check-equal? (s:apply (up (λ (x) x)) '(2)) (up 2))
    (check-equal? (s:apply (down (λ (x) (expt x 2))) '(2)) (down 4)))
   (test-case
    "abstract..."
    (check-equal? (as:arity (abstract-down 'x)) (arity-at-least 0))
    (let ([A (abstract-up 'X)])
      (add-property! A 'arity 4)
      (check-equal? (as:arity A) 4))
    (check-true ((has-property? 'zero) (ac:zero-like (up 2 (down 3)))))
    (check-true ((has-property? 'zero) (ar:zero-like (down 2 (up 3)))))
    (check-equal? (expression ((make-up-combination cons) (abstract-up 'A) (abstract-up 'B)))
                  '(cons A B))
    (check-equal? (expression ((make-up-combination cons #t) (abstract-up 'A) (abstract-up 'B)))
                  '(cons B A))
    (check-equal? (expression ((make-up-combination 'negate) (abstract-up 'A)))
                  '(negate A))
    (check-equal? (expression ((make-down-combination cons) (abstract-down 'A) (abstract-down 'B)))
                  '(cons A B))
    (check-equal? (expression ((make-down-combination cons #t) (abstract-down 'A) (abstract-down 'B)))
                  '(cons B A))
    (check-equal? (expression ((make-down-combination 'negate) (abstract-down 'A)))
                  '(negate A)))
   (test-case
    "~matrix"
    (check-equal? (submatrix (up (down 1 2 3 4 5)
                                 (down 6 7 8 9 10)
                                 (down 11 12 13 14 15)
                                 (down 16 17 18 19 20))
                             1 3 0 3)
                  (matrix-by-rows '(6 7 8) '(11 12 13)))
    (check-equal? (submatrix (matrix-by-rows '(1 2 3 4 5)
                                             '(6 7 8 9 10)
                                             '(11 12 13 14 15)
                                             '(16 17 18 19 20))
                             1 3 0 3)
                  (matrix-by-rows '(6 7 8) '(11 12 13)))
    (check-exn #px"Wrong type submatrix" (λ () (submatrix 'any 1 3 0 3))))
   (test-case
    "s->m / m->s"
    (define (chck-mid L M R V [M* M])
      (check-equal? (s->m L M R) V)
      (check-equal? (m->s R V L) M*))
    (chck-mid 1/3 1 3 (matrix-by-rows '(1)))
    (chck-mid 1/3 (down 1) (up 3) (matrix-by-rows '(1)))
    (chck-mid (down (down 1/3)) (up 1) (up 3) (matrix-by-rows '(1)) (up (up (down 1))))
    (parameterize ([*careful-conversion* #t])
      (check-exn #px"Innapropriate s->m" (λ () (s->m (up 1) (up 1) (up 1))))
      (check-exn #px"Innapropriate m->s" (λ () (m->s (up 1) (matrix-by-rows (list (up 1))) (up 1)))))
    (parameterize ([*careful-conversion* #f])
      (chck-mid (up 1) (up 1) (up 1) (matrix-by-rows (list (up (up (up 1))))) (down (down (up (up (up 1))))))))
   (test-case
    "transpose"
    (check-equal? (s:transpose (up 'a 'b) (down (down 'c 'd) (down 'e 'f) (down 'g 'h)) (up 'i 'j 'k))
                  (down (down 'c 'e 'g) (down 'd 'f 'h)))
    (check-equal? (s:transpose1 (down (down 'c 'd) (down 'e 'f) (down 'g 'h)) (up 'i 'j 'k))
                  (down (down 'c 'e 'g) (down 'd 'f 'h)))
    (check-equal? (s:transpose-outer (down (down (up 'x 'y) (up 'z 'w))
                                           (down (up 'a 'b) (up 'c 'd))))
                  (down (down (up 'x 'y) (up 'a 'b))
                        (down (up 'z 'w) (up 'c 'd)))))
   (test-case
    "inverse"
    (check-equal? (s:inverse (up 'a 'b) (down (down 1 3) (down 1 5)) (up 'i 'j))
                  (up (up 5/2 -3/2) (up -1/2 1/2)))
    (check-equal? (s:inverse1 (down (down 1 3) (down 1 5)) (up 'i 'j))
                  (up (up 5/2 -3/2) (up -1/2 1/2))))
   (test-case
    "turtles, all the way _"
    (check-equal? (A_mn->Mnm (down (down 1 2 3) (down 4 5 6)))
                  (matrix-by-rows '(1 2 3) '(4 5 6)))
    (check-equal? (Mnm->A_mn (matrix-by-rows '(1 2 3) '(4 5 6)))
                  (down (down 1 2 3) (down 4 5 6)))
    (check-exn #px"Not A_mn -- A_mn->Mnm" (λ () (A_mn->Mnm (down (down 1 2 3) (up 4 5 6)))))
    (check-exn #px"Not A_mn -- A_mn->Mnm" (λ () (A_mn->Mnm (down (down 1 2 3) (down 3 4 5 6)))))
    (check-exn #px"Not A_mn -- A_mn->Mnm" (λ () (A_mn->Mnm (up (down 1 2 3) (down 4 5 6)))))
    (check-exn #px"Not a matrix -- Mnm->A_mn" (λ () (Mnm->A_mn 'any))))
   (test-case
    "only way left is ^"
    (check-equal? (A^mn->Mmn (up (up 1 2 3) (up 4 5 6)))
                  (matrix-by-rows '(1 4) '(2 5) '(3 6)))
    (check-equal? (Mmn->A^mn (matrix-by-rows '(1 4) '(2 5) '(3 6)))
                  (up (up 1 2 3) (up 4 5 6)))
    (check-exn #px"Not A\\^mn -- A\\^mn->Mmn" (λ () (A^mn->Mmn (up (down 1 2 3) (up 4 5 6)))))
    (check-exn #px"Not A\\^mn -- A\\^mn->Mmn" (λ () (A^mn->Mmn (up (up 1 2 3) (up 3 4 5 6)))))
    (check-exn #px"Not A\\^mn -- A\\^mn->Mmn" (λ () (A^mn->Mmn (down (up 1 2 3) (up 4 5 6)))))
    (check-exn #px"Not a matrix -- Mmn->A\\^mn" (λ () (Mmn->A^mn 'any))))
   (test-case
    "A^m_n"
    (check-equal? (A^m_n->Mmn (down (up 1 2 3) (up 4 5 6)))
                  (matrix-by-rows '(1 4) '(2 5) '(3 6)))
    (check-equal? (Mmn->A^m_n (matrix-by-rows '(1 4) '(2 5) '(3 6)))
                  (down (up 1 2 3) (up 4 5 6)))
    (check-exn #px"Not A\\^m_n -- A\\^m_n->Mmn" (λ () (A^m_n->Mmn (down (down 1 2 3) (up 4 5 6)))))
    (check-exn #px"Not A\\^m_n -- A\\^m_n->Mmn" (λ () (A^m_n->Mmn (down (up 1 2 3) (up 3 4 5 6)))))
    (check-exn #px"Not A\\^m_n -- A\\^m_n->Mmn" (λ () (A^m_n->Mmn (up (down 1 2 3) (down 4 5 6)))))
    (check-exn #px"Not a matrix -- Mmn->A\\^m_n" (λ () (Mmn->A^m_n 'any))))
   (test-case
    "A_m^n"
    (check-equal? (A_m^n->Mnm (up (down 1 2 3) (down 4 5 6)))
                  (matrix-by-rows '(1 2 3) '(4 5 6)))
    (check-equal? (Mnm->A_m^n (matrix-by-rows '(1 2 3) '(4 5 6)))
                  (up (down 1 2 3) (down 4 5 6)))
    (check-exn #px"Not A_m\\^n -- A_m\\^n->Mnm" (λ () (A_m^n->Mnm (up (down 1 2 3) (up 4 5 6)))))
    (check-exn #px"Not A_m\\^n -- A_m\\^n->Mnm" (λ () (A_m^n->Mnm (up (down 1 2 3) (down 3 4 5 6)))))
    (check-exn #px"Not A_m\\^n -- A_m\\^n->Mnm" (λ () (A_m^n->Mnm (down (up 1 2 3) (up 4 5 6)))))
    (check-exn #px"Not a matrix -- Mnm->A_m\\^n" (λ () (Mnm->A_m^n 'any))))
   (test-case
    "tensor"
    (define DD (down (down) (down)))
    (define DU (down (up) (up)))
    (define UD (up (down) (down)))
    (define UU (up (up) (up)))
    (check-true (2-down? DD))
    (check-false (2-down? DU))
    (check-false (2-down? UD))
    (check-true (2-up? UU))
    (check-false (2-up? UD))
    (check-false (2-up? DU))
    (check-true (up-of-downs? UD))
    (check-false (up-of-downs? UU))
    (check-false (up-of-downs? DD))
    (check-true (down-of-ups? DU))
    (check-false (down-of-ups? DD))
    (check-false (down-of-ups? UU))
    (skip ;!!!
     (check-false (2-down? (down (down) (up))))
     (check-false (2-up? (up (up) (down))))
     (check-false (up-of-downs? (up (down) (up))))
     (check-false (down-of-ups? (down (up) (down)))))
    (check-true (andmap 2-tensor? (list DD DU UD UU))))
   (test-case
    "single-layer-struct"
    (check-true (single-layer-down? (down 1 2 3 4)))
    (check-false (single-layer-down? (down 1 2 3 (up 4))))
    (check-false (single-layer-down? (up 1 2 3 4)))
    (check-true (single-layer-up? (up 1 2 3 4)))
    (check-false (single-layer-down? (up 1 2 3 (down 4))))
    (check-false (single-layer-up? (down 1 2 3 4))))
   (test-case
    "struc->matrix / invert"
    (check-equal? (structure->matrix (down (down 1 2) (down 3 4))) (matrix-by-rows '(1 2) '(3 4)))
    (check-equal? (structure->matrix (up (down 1 2) (down 3 4))) (matrix-by-rows '(1 2) '(3 4)))
    (check-equal? (structure->matrix (down (up 1 2) (up 3 4))) (matrix-by-rows '(1 3) '(2 4)))
    (check-equal? (structure->matrix (up (up 1 2) (up 3 4))) (matrix-by-rows '(1 3) '(2 4)))
    (check-exn #px"structure->matrix" (λ () (structure->matrix 'any)))
    
    (check-equal? (s:invert (down (down 1 2) (down 3 4))) (up (up -2 1) (up 3/2 -1/2)))
    (check-equal? (s:invert (up (down 1 2) (down 3 4))) (up (down -2 1) (down 3/2 -1/2)))
    (check-equal? (s:invert (down (up 1 2) (up 3 4))) (down (up -2 1) (up 3/2 -1/2)))
    (check-equal? (s:invert (up (up 1 2) (up 3 4))) (down (down -2 1) (down 3/2 -1/2)))
    (check-exn #px"s:invert" (λ () (s:invert 'any))))
   (test-case
    "/tenser"
    (check-equal? (scalar/tensor 3 (up (down 3))) (up (down 1)))
    (check-equal? (scalar/tensor 2 (down (down 3 4) (down 1/2 1))) (up (up 2 -8) (up -1 6))))
   (test-case
    "solve"
    (check-equal? (s:solve-linear-left (up 1) 4) (down 4))
    (check-equal? (s:solve-linear-left (up 2) 4) (down 2))
    (check-equal? (s:solve-linear-left (up (up 2)) 4) (down (down 2)))
    (check-equal? (s:solve-linear-left (up (up 2)) 4) (down (down 2)))
    (check-equal? (s:solve-linear-left (up (up 2)) (up 4)) (down 2))
    (check-equal? (s:solve-linear-left (up (down 2 3) (down 2 9)) (down 4 8)) (down 5/3 1/3))
    (check-equal? (s:solve-linear-right (down 4 8) (down (up 2 3) (up 2 9))) (down 1 2/3))
    (check-equal? (s:divide-by-structure (down 4 8) (up (down 2 3) (down 2 9))) (down 5/3 1/3))
    (local-require "../../kernel-intr.rkt")
    (check-equal? (g:solve-linear-left (up (down 2 3) (down 2 9)) 5) (up (down 15/4 -5/4) (down -5/6 5/6)))
    (check-equal? (g:solve-linear (up (down 2 3) (down 2 9)) 5) (up (down 15/4 -5/4) (down -5/6 5/6))))
   (test-case
    "determinant / trace"
    (check-equal? (s:determinant (up (down 1 2) (down 3 4))) -2)
    (check-equal? (s:trace (up (down 1 2) (down 3 4))) 5))
   (test-case
    "flip / typ / acc / proto"
    (check-equal? (flip-indices (up 1 2 (down 3 (up 4)))) (down 1 2 (up 3 (down 4))))
    (check-equal? (flip-outer-index (up 1 2 (down 3 (up 4)))) (down 1 2 (down 3 (up 4))))
    (check-unique-match? (expression (typical-object (up 1 2 (down 3 (up 4)))))
                         (a b c d)
                         `(up ,a ,b (down ,c (up ,d))))
    (check-unique-match? (expression (typical-object 'any))
                         (a) a)
    (check-equal? (structure->access-chains (up 1 2 (down 3 (up 4))))
                  (up '(0) '(1) (down '(2 0) (up '(2 1 0)))))
    (check-equal? (structure->prototype 'f (up 1 2 (down 3 (up 4))))
                  (up 'f:0 'f:1 (down 'f:2:0 (up 'f:2:1:0)))))
   (test-case
    "compat- ."
    (check-equal? (compatible-zero (up 1 2 (down 3 (up 4)))) (down 0 0 (up 0 (down 0))))
    (check-equal? (compatible-zero 'any) 0)
    (check-unique-match? (expression (compatible-shape (up 1 2 (down 3 (up 4)))))
                         (a b c d)
                         `(down ,a ,b (up ,c (down ,d))))
    (check-unique-match? (expression (compatible-shape 'any))
                         (a) a))
   (test-case
    "contract"
    (define (s:gen^x specs [f (λ _ 0)])
      (define len (length specs))
      (define (lp spcs prv)
        (λ (I) (if (null? (cdr spcs))
                   (s:generate len (car spcs) (λ (i) (apply f `(,@prv ,I ,i))))
                   (s:generate len (car spcs) (lp (cdr spcs) `(,@prv ,I))))))
      (s:generate len (car specs) (lp (cdr specs) '())))
    (define S2 (structure->prototype 'i (s:gen^x '(down up))))
    (check-equal? (expression (s:contract S2 0 1)) '(+ i:0:0 i:1:1))
    (check-equal? (s:contract (down (up 000 001) (up 104 105)) 1 0) 105)
    (define S3 (structure->prototype 'i (s:gen^x '(up up down))))
    (check-equal? (expression (s:contract S3 0 2))
                  '(up (+ i:0:0:0 i:1:0:1 i:2:0:2) (+ i:0:1:0 i:1:1:1 i:2:1:2) (+ i:0:2:0 i:1:2:1 i:2:2:2)))
    (check-equal? (expression (s:contract S3 1 2))
                  '(up (+ i:0:0:0 i:0:1:1 i:0:2:2) (+ i:1:0:0 i:1:1:1 i:1:2:2) (+ i:2:0:0 i:2:1:1 i:2:2:2)))
    (check-equal? (expression (s:contract S3 2 1))
                  '(up (+ i:0:0:0 i:0:1:1 i:0:2:2) (+ i:1:0:0 i:1:1:1 i:1:2:2) (+ i:2:0:0 i:2:1:1 i:2:2:2)))
    (check-exn #px"assertion failed: \\(not \\(eq\\?" (λ () (s:contract S3 0 1)))
    ;; !!! I don't understand why it is important to contract on a up+down combination
    ;;     when it is more important to check that for the indexes the length is "="
    (define S4 (s:gen^x '(up down up down) +))
    (check-equal? (expression (s:contract S4 0 1))
                  '(up (down 12 16 20 24)
                       (down 16 20 24 28)
                       (down 20 24 28 32)
                       (down 24 28 32 36))))
   (test-case
    "as-matrix"
    (check-equal? ((as-matrix flip-indices) (up 1 2 (down 3 4)))
                  (matrix-by-rows '(1 2 3 4)))
    (check-equal? ((as-matrix (λ (s) (structure*scalar s 5))) (down 1 (up 3)))
                  (matrix-by-rows '(5 0) '(15 0) '(0 5) '(0 15))))
   

   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))