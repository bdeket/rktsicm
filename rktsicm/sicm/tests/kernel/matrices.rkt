#lang s-exp "../../kernel/extapply.rkt"

(require rackunit
         (only-in "../../kernel.rkt" expression g:ref g:partial-derivative g:zero? g:one? g:arity g:+) ;; necessary to load generic operators
         "../../kernel/matrices.rkt"
         (only-in "../../kernel/structs.rkt" up down s:ref)
         "../../kernel/types.rkt"
         "../helper.rkt"
         )

(define (dd i j) (+ (* 10 i) j))

(provide the-tests)
(define the-tests
  (test-suite
   "kernel/matrices"
   ;; CSTM
   (test-case
    "type"
    (check-equal? (m:type 'any) matrix-type-tag)
    (check-equal? (m:type-predicate 'any) matrix-quantity?)
    (check-true ((m:type-predicate 'any) (tag-matrix 1 1 '((1)))))
    (define M (tag-matrix 1 2 '((3 4))))
    (check-equal? (m:num-rows M) 1)
    (check-equal? (m:num-cols M) 2)
    (check-equal? (matrix->array M) '((3 4))))
   (test-case
    "m:dimension"
    (check-exn #px"Not a matrix -- DIMENSION" (λ () (m:dimension 'any)))
    (check-exn #px"Not a square matrix -- DIMENSION" (λ () (m:dimension (tag-matrix 1 2 '((1 2))))))
    (check-equal? (m:dimension (tag-matrix 2 2 '((1 2)(2 2)))) 2)

    (check-exn #px"Not a matrix -- SIZE" (λ () (matrix-size 'any)))
    (check-equal? (matrix-size (matrix-by-rows '(1 2 3) '(4 5 6))) 6)
    (check-equal? (matrix-size (literal-matrix 'M 6 7)) 42))
   (test-case
    "col/row-matrix"
    (check-true (column-matrix? (literal-matrix 'M 6 1)))
    (check-false (column-matrix? (literal-matrix 'M 1 6)))
    (check-false (column-matrix? 'any))
    (check-true (row-matrix? (literal-matrix 'M 1 6)))
    (check-false (row-matrix? (literal-matrix 'M 6 1)))
    (check-false (row-matrix? 'any))
    (check-equal? (column-matrix->up (up->column-matrix (up 1 2 3))) (up 1 2 3))
    (check-equal? (row-matrix->down (down->row-matrix (down 1 2 3))) (down 1 2 3))
    (let ([V (column-matrix->up (literal-column-matrix 'l 3))])
      (check-true (vector? V))
      (check-true (for/and ([v (in-vector V)]) (symbol? v))))
    (let ([V (row-matrix->down (literal-row-matrix 'l 2))])
      (check-true (down? V))
      (check-true (for/and ([i (in-range 2)]) (symbol? (s:ref V i))))))
   (test-case
    "matrix-ref"
    (check-equal? (matrix-ref (matrix-by-rows '(1 2 3) '(4 5 6)) 0 0) 1)
    (check-equal? (matrix-ref (matrix-by-cols '(1 4) '(2 5) '(3 6)) 1 1) 5)
    (check-equal? (m:ref (matrix-by-rows '(1 2 3) '(4 5 6)) 1 2) 6)
    (check-equal? (m:ref (m:generate 4 4 *) 2 3) (* 2 3)))
   (test-case
    "submatrix"
    (check-equal? (m:submatrix (m:generate 5 5 +) 2 4 1 5)
                  (m:generate 2 4 (λ (r c) (+ (+ r 2) (+ c 1))))))
   ;; MAIN
   (test-case
    "matrix-generation"
    (check-exn #px"Not an array -- ARRAY->MATRIX" (λ () (array->matrix #(#(1 2) (1 2 3)))))
    (check-exn #px"Not all rows have same length -- ARRAY->MATRIX" (λ () (array->matrix #(#(1 2) #(1 2 3)))))
    (check-equal? (array->matrix #(#(0 1)#(1 2))) (m:generate 2 2 +))

    (check-exn #rx"assertion failed: \\(vector\\? v\\)" (λ () (vector->row-matrix '(1 2 3))))
    (check-exn #rx"assertion failed: \\(vector\\? v\\)" (λ () (vector->column-matrix '(1 2 3))))
    (check-equal? (vector->row-matrix #(0 1 2)) (m:generate 1 3 +))
    (check-equal? (vector->column-matrix #(0 1 2)) (m:generate 3 1 +))
    (check-equal? (row-matrix 0 1 2) (m:generate 1 3 +))
    (check-equal? (column-matrix 0 1 2) (m:generate 3 1 +))

    (check-equal? (column-matrix->vector (m:generate 3 1 +)) #(0 1 2))
    (check-equal? (row-matrix->vector (m:generate 1 3 +)) #(0 1 2))
    (check-exn #rx"assertion failed: \\(column-matrix\\? m\\)" (λ () (column-matrix->vector (m:generate 1 3 +))))
    (check-exn #rx"assertion failed: \\(row-matrix\\? m\\)" (λ () (row-matrix->vector (m:generate 3 1 +))))

    (check-equal? (m:nth-col (m:generate 2 3 +) 2) #(2 3))
    (check-equal? (m:nth-row (m:generate 2 3 +) 1) #(1 2 3))
    (check-equal? (m:diagonal (m:generate 3 3 +)) #(0 2 4))
    (check-equal? (m:diagonal (m:make-diagonal #(1 2 3))) #(1 2 3))
    (check-true (diagonal? (m:make-diagonal #(1 2 3 4))))
    (check-true (diagonal? (m:make-identity 3)))
    (check-false (diagonal? (matrix-by-rows '(1 0 0) '(0 1 1) '(0 0 1))))
    (check-exn #rx"Not a matrix -- DIAGONAL\\?" (λ () (diagonal? 'any))))
   (test-case
    "switching"
    (check-equal? (matrix-with-substituted-row (matrix-by-rows '(1 2 3) '(4 5 6)) 1 #(2 4 8))
                  (matrix-by-rows '(1 2 3) '(2 4 8)))
    (check-equal? (m:transpose (matrix-by-rows '(1 2 3) '(4 5 6)))
                  (matrix-by-cols '(1 2 3) '(4 5 6)))
    (check-equal? (m:minor (m:generate 4 4 dd) 1 1)
                  (matrix-by-rows '(00 02 03) '(20 22 23) '(30 32 33))))
   (test-case
    "element-wise"
    (check-equal? ((m:elementwise +) (m:generate 2 3 +) (m:generate 2 3 *))
                  (m:generate 2 3 (λ (i j) (+ (+ i j) (* i j))))))
   (test-case
    "zero/one"
    (check-false (m:zero? (m:generate 2 3 +)))
    (check-true  (m:zero? (m:generate 6 2 (λ _ 0))))
    (check-true  (m:zero? (m:make-zero 2 4)))
    (check-true  (m:zero? (m:make-zero 2)))
    (check-true  (m:zero? (m:zero-like (m:generate 2 3 +))))
    (check-exn #rx"Not a matrix -- ZERO\\?" (λ () (m:zero? 'any)))
    (check-equal? (matrix-size (m:zero-like (m:generate 2 3 +))) 6)
    (check-equal? (m:make-identity 3) (matrix-by-rows '(1 0 0) '(0 1 0) '(0 0 1)))
    (check-false (m:identity? (m:generate 2 3 +)))
    (check-false (m:identity? (m:generate 3 3 (λ _ 1))))
    (check-false (m:identity? (m:generate 3 3 (λ (r c) (if (= r c) r 0)))))
    (check-false (m:identity? (matrix-by-rows '(1 0 0) '(0 1 0))))
    (check-true (m:identity? (m:make-identity 3)))
    (check-true (m:identity? (m:one-like (m:generate 3 3 +))))
    (check-true (m:identity? (m:identity-like (m:generate 2 2 +))))
    (check-exn #rx"Not a matrix -- IDENTITY\\?" (λ () (m:identity? 'any)))
    (check-equal? (m:dimension (m:zero-like (m:generate 3 3 +))) 3))
   (test-case
    "="
    (check-true (matrix=matrix (m:generate 3 3 +) (m:generate 3 3 +)))
    (check-false (matrix=matrix (m:generate 3 3 +) (m:generate 3 3 *)))
    (check-false (matrix=matrix (m:generate 3 3 +) (m:generate 3 2 *))))
   (test-case
    "+ / -"
    (check-equal? (matrix+matrix (m:generate 3 3 +) (m:generate 3 3 *))
                  (m:generate 3 3 (λ (i j) (+ (+ i j) (* i j)))))
    (check-exn #px"Not a matrix --" (λ () (matrix-matrix (m:generate 3 3 +) 'any)))
    (check-exn #px"Matrices of unequal size --" (λ () (matrix-matrix (m:generate 3 3 +) (m:generate 2 3 +))))
    (check-true (m:zero? (matrix-matrix (m:generate 3 3 +) (m:generate 3 3 +)))))
   (test-case
    "*"
    (check-equal? (matrix*matrix (m:generate 1 2 +) (m:generate 2 1 +)) (matrix-by-rows '(1)))
    (check-equal? (matrix*matrix (m:generate 2 1 +) (m:generate 1 2 +)) (matrix-by-rows '(0 0) '(0 1)))
    (check-exn #px"Not a matrix -- \\*" (λ () (matrix*matrix (m:generate 3 3 +) 'any)))
    (check-exn #px"Matrix sizes do not match -- MATRIX\\*MATRIX" (λ () (matrix*matrix (m:generate 3 3 +) (m:generate 2 3 +))))
    (check-equal? (m:square (matrix-by-rows '(2))) (matrix-by-rows '(4)))
    (check-equal? (matrix*up (m:generate 2 2 dd) (up 3 2)) (up 2 52))
    (check-equal? (matrix*vector (m:generate 2 2 dd) (up 3 2)) (up 2 52))
    (check-equal? (down*matrix (down 3 2) (m:generate 2 2 dd)) (down 20 25))
    (check-equal? (vector*matrix (vector 3 2) (m:generate 2 2 dd)) (vector 20 25)))
   (test-case
    "expt"
    (check-equal? (m:expt (m:generate 2 2 +) 1) (m:generate 2 2 +))
    (check-equal? (m:expt (m:generate 2 2 +) 2)
                  (matrix*matrix (m:generate 2 2 +) (m:generate 2 2 +)))
    (check-equal? (m:expt (m:generate 2 2 +) 3)
                  (matrix*matrix (matrix*matrix (m:generate 2 2 +) (m:generate 2 2 +))
                                 (m:generate 2 2 +)))
    (check-equal? (m:expt (m:make-identity 3) -1) (m:make-identity 3))
    (check-equal? (m:expt (m:generate 3 3 +) 0) (m:make-identity 3))
    (check-exn #px"Not a matrix -- EXPT" (λ () (m:expt 'any 2)))
    (check-exn #px"Only integer powers allowed -- M:EXPT" (λ () (m:expt (m:generate 3 3 +) 1/2))))
   (test-case
    "M*s"
    (check-equal? (matrix*scalar (m:generate 2 2 dd) 2) (matrix-by-rows '(0 2) '(20 22)))
    (check-exn #px"Not matrix\\*scalar" (λ () (matrix*scalar (m:generate 2 2 +) (m:generate 2 2 +))))
    (check-equal? (scalar*matrix 3 (m:generate 2 2 dd)) (matrix-by-rows '(0 3) '(30 33)))
    (check-exn #px"Not matrix\\*scalar" (λ () (scalar*matrix (m:generate 2 2 +) (m:generate 2 2 +))))
    (check-equal? ((m:scale 4) (m:generate 2 2 dd)) (matrix-by-rows '(0 4) '(40 44)))
    (check-equal? (matrix/scalar (m:generate 2 2 dd) 2) (matrix-by-rows '(0 1/2) '(5 11/2)))
    (check-equal? (scalar/matrix 3 (m:generate 2 2 dd)) (matrix-by-rows '(-33/10 3/10) '(3 0))))
   (test-case
    "outer/innder-prod"
    (check-equal? (m:outer-product (column-matrix 1 2) (row-matrix 3 4)) (matrix-by-rows '(3 4) '(6 8)))
    (check-exn #px"assertion failed" (λ () (m:outer-product (column-matrix 1 2) (column-matrix 3 4))))
    (check-equal? (m:inner-product (row-matrix 3 4) (column-matrix 1 2)) 11)
    (check-exn #px"assertion failed" (λ () (m:inner-product (column-matrix 1 2) (row-matrix 3 4))))
    (check-equal? (m:dot-product-row (row-matrix 1 2) (row-matrix 1 3)) 7)
    (check-equal? (m:dot-product-column (column-matrix 1 2) (column-matrix 1 3)) 7)
    (check-equal? (m:cross-product-row (row-matrix 1 2 6) (row-matrix 1 3 1)) (row-matrix -16 5 1))
    (check-equal? (m:cross-product-column (column-matrix 1 2 6) (column-matrix 1 3 1)) (column-matrix -16 5 1)))
   (test-case
    "/"
    (check-equal? (matrix/matrix (row-matrix 2) (row-matrix 3)) (row-matrix 2/3))
    (check-equal? (matrix/matrix (m:generate 2 2 +) (m:generate 2 2 dd)) (matrix-by-rows '(1 0) '(9/10 1/10))))
   (test-case
    "matrix = + - scalar"
    (check-true  (matrix=scalar (matrix-by-rows '(3 0) '(0 3)) 3))
    (check-false (matrix=scalar (m:generate 2 2 dd) 3))
    (check-true  (scalar=matrix 3 (matrix-by-rows '(3 0) '(0 3))))
    (check-false (scalar=matrix 3 (m:generate 2 2 dd)))
    (check-equal? (matrix+scalar (m:generate 2 2 dd) 3)
                  (matrix-by-rows '(3 1) '(10 14)))
    (check-equal? (scalar+matrix 3 (m:generate 2 2 dd))
                  (matrix-by-rows '(3 1) '(10 14)))
    (check-equal? (matrix-scalar (m:generate 2 2 dd) 3)
                  (matrix-by-rows '(-3 1) '(10 8)))
    (check-equal? (scalar-matrix 3 (m:generate 2 2 dd))
                  (matrix-by-rows '(3 -1) '(-10 -8))))
   (test-case
    "trace"
    (check-equal? (m:trace (m:generate 2 2 dd)) 11)
    (check-equal? (m:trace (m:generate 3 3 dd)) 33)
    (check-exn #px"Not a matrix -- TRACE" (λ () (m:trace 'any)))
    (check-exn #px"Not a square matrix -- TRACE" (λ () (m:trace (m:generate 2 3 dd)))))
   (test-case
    "conj / neg"
    (check-equal? (m:conjugate (row-matrix 2 1+4i)) (row-matrix 2 1-4i))
    (check-equal? (m:negate (row-matrix 2 5)) (row-matrix -2 -5)))
   (test-case
    "series"
    (check-true (series? (m:exp (m:generate 2 2 dd))))
    (check-equal? (g:ref (m:exp (m:generate 2 2 dd)) 0) (matrix-by-rows '(1 0) '(0 1)))
    (check-equal? (g:ref (m:exp (m:generate 2 2 dd)) 2) (matrix-by-rows '(5 11/2) '(55 131/2)))
    (check-true (series? (m:sin (m:generate 2 2 dd))))
    (check-equal? (g:ref (m:sin (m:generate 2 2 dd)) 0) (matrix-by-rows '(0 0) '(0 0)))
    (check-equal? (g:ref (m:sin (m:generate 2 2 dd)) 1) (matrix-by-rows '(0 1) '(10 11)))
    (check-true (series? (m:cos (m:generate 2 2 dd))))
    (check-equal? (g:ref (m:cos (m:generate 2 2 dd)) 0) (matrix-by-rows '(1 0) '(0 1)))
    (check-equal? (g:ref (m:cos (m:generate 2 2 dd)) 1) (matrix-by-rows '(0 0) '(0 0))))
   (test-case
    "invert"
    (check-equal? (expression (m:invert (matrix-by-rows '(d)))) '(matrix-by-rows (list (/ 1 d))))
    (check-equal? (expression (m:invert (matrix-by-rows '(a b) '(c d))))
                  '(matrix-by-rows
                    (list (/ d (- (* a d) (* b c))) (/ b (- (- (* a d) (* b c)))))
                    (list (/ c (- (- (* a d) (* b c)))) (/ a (- (* a d) (* b c)))))))
   (test-case
    "determinant"
    (check-equal? (expression (m:determinant (matrix-by-rows '(d)))) 'd)
    (check-equal? (expression (m:determinant (matrix-by-rows '(a b) '(c d))))
                  '(- (* a d) (* b c))))
   (test-case
    "solve"
    (check-equal? (expression (m:solve (matrix-by-rows '(a)) (matrix-by-rows '(b))))
                  '(matrix-by-rows (list (/ b a))))
    (check-equal? (expression (m:solve (matrix-by-rows '(a b) '(c d)) (column-matrix 'e 'f)))
                  '(matrix-by-rows
                    (list (/ (- (* e d) (* f b)) (- (* a d) (* b c))))
                    (list (/ (- (* a f) (* c e)) (- (* a d) (* b c)))))))
   (test-case
    "numerical?"
    (skip "this is unsafe, easy to break")
    (define-values (A B C) (values (gensym) (gensym) (gensym)))
    (set-numerical! (λ _ A) (λ _ B) (λ _ C))
    (check-equal? (m:invert (m:generate 3 3 dd)) A)
    (check-equal? (m:solve (m:generate 3 3 dd) (column-matrix 1 2 3)) B)
    (check-equal? (m:determinant (m:generate 3 3 dd)) C)
    (set-symbolic!)
    (check-equal? (m:invert (row-matrix 1)) (row-matrix 1))
    (check-equal? (m:determinant (row-matrix 1)) 1)
    (check-equal? (m:solve (row-matrix 1) (row-matrix 1)) (row-matrix 1)))
   (test-case
    "rsolve / linear"
    (check-equal? (m:rsolve (column-matrix 7 9) (m:generate 2 2 dd))
                  (matrix-by-rows '(-34/5) '(7)))
    (check-equal? (m:rsolve (up 7 9) (m:generate 2 2 dd))
                  (up -34/5 7))
    (check-equal? (m:rsolve (down 7 9) (m:generate 2 2 dd))
                  (down 13/10 7/10))
    (check-equal? (m:rsolve (row-matrix 7 9) (m:generate 2 2 dd))
                  (matrix-by-rows '(13/10 7/10)))
    (check-exn #px"I don't know how to solve:" (λ () (m:solve-linear (m:generate 2 2 dd) (list 7 9)))))
   (test-case
    "apply"
    (check-equal? (m:apply (m:generate 2 2 (λ (i j) (λ (a b) (list i a j b)))) '(1 2))
                  (matrix-by-rows '((0 1 0 2) (0 1 1 2))
                                  '((1 1 0 2) (1 1 1 2)))))
   (test-case
    "arity"
    (check-equal? (m:arity (matrix-by-rows (list (λ _ 4) (case-lambda [(a) 1][(a b c) 2]))
                                           (list (λ (a . b) 3) (λ (a b c) 3))))
                  3)
    (check-equal? (m:arity (matrix-by-rows (list (λ _ 4) (case-lambda [(a) 1][(a b c) 2]))
                                           (list (λ (a . b) 3) (λ (a b c d) 3))))
                  #f))
   (test-case
    "m:inexact?"
    (check-false (m:inexact? (matrix-by-rows '(1 2) '(1 3))))
    (check-true (m:inexact? (matrix-by-rows '(1 2) '(1. 3)))))
   (test-case
    "m:partial-derivative"
    (check-equal? (m:apply (m:partial-derivative (matrix-by-rows (list (lambda (x y) x))) '(0)) (list (up 's) (up 't)))
                  (m:apply (matrix-by-rows (list (g:partial-derivative (lambda (x y) x) 0))) (list (up 's) (up 't)))))

   ;; Abstract matrices (not literal!)
   (test-case
    "make-abstract"
    (check-false (matrix? (abstract-matrix 'M)))
    (check-true  (abstract-matrix? (abstract-matrix 'M)))
    (check-equal? (expression (abstract-matrix 'M)) 'M)
    (check-equal? (am:arity (abstract-matrix 'M)) (arity-at-least 0))
    (check-false (g:zero? (abstract-matrix 'M)))
    (check-true (g:zero? (am:zero-like 'M)))
    (check-false (g:one? (abstract-matrix 'M)))
    (check-true (g:one? (am:one-like 'M)))
    (check-false (g:one? (abstract-matrix 'M)))
    (check-true (g:one? (am:id-like 'M)))
    (check-equal? (expression (g:+ (abstract-matrix 'M) (abstract-matrix 'N)))
                  '(+ M N))
    (check-equal? (expression ((make-matrix-combination 'rev+ #t) (abstract-matrix 'M) (abstract-matrix 'N)))
                  '(rev+ N M)))

   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))