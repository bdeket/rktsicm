#lang racket/base

(require rackunit
         "../../main.rkt"
         "../../calculus/indexed.rkt"
         "../helper+scm.rkt"
         )

(provide the-tests)
(define the-tests
  (test-suite
   "calculus/indexed"
   ;; indexed/types
   (test-case
    "argument-types"
    (define (f x) x)
    (define g f)
    (check-false (has-argument-types? f))
    (declare-argument-types! f (list number?))
    (check-equal? (argument-types f) (list number?))
    (check-true (eq? f g))
    (check-not-false (has-argument-types? g))
    (check-exn #px"assertion failed: \\(procedure\\? proc\\)"
               (λ () (declare-argument-types! 'not-a-proc (list number?)))))
   (test-case
    "index-types"
    (define (f x) x)
    (define g f)
    (check-false (has-index-types? f))
    (declare-index-types! f (list number?))
    (check-equal? (index-types f) (list number?))
    (check-true (eq? f g))
    (check-not-false (has-index-types? g))
    (check-exn #px"assertion failed: \\(procedure\\? proc\\)"
               (λ () (declare-index-types! 'not-a-proc (list number?)))))

   ;; indexed
   (test-case
    "typed->indexed"
    (define-coordinates (up x y) R2-rect)
    (define basis (coordinate-system->basis R2-rect))
    (define pt ((point R2-rect) #(x y)))

    ;; zero arguments is valid, but is it usefull?
    (define H (declare-argument-types! (λ () (λ (pt) pt)) '()))
    (check-equal? (((typed->indexed H basis) '()) pt) pt)
    
    (define (F v) (+ (* 2 (dy v)) (dx v)))
    (declare-argument-types! F (list vector-field?))
    (check-equal? (((typed->indexed F basis) '(0)) pt) 1)
    (check-equal? (((typed->indexed F basis) '(1)) pt) 2)
    ;; only a 1form is acceptable
    (check-not-exn (λ () (typed->indexed (declare-argument-types! F (list 1form-field?)) basis)))

    (define (G w v) (+ (* 2 (w d/dy)) (dx v)))
    (declare-argument-types! G (list 1form-field? vector-field?))
    (check-equal? (index-types (typed->indexed G basis)) (list up down))
    (check-equal? (((typed->indexed G basis) '(0 0)) pt) 1)
    (check-equal? (((typed->indexed G basis) '(0 1)) pt) 0)
    (check-equal? (((typed->indexed G basis) '(1 0)) pt) 3)
    (check-equal? (((typed->indexed G basis) '(1 1)) pt) 2)

    (check-exn #px"Bad arg types:\n\tassertion failed: " ;; must have arguments
               (λ () (typed->indexed (λ (x) x) basis)))
    (check-exn #px"Bad arg types:\n\tassertion failed: " ;; arguments must be vector-field? or 1form-field
               (λ () (typed->indexed (declare-argument-types! G (list number? procedure?)) basis)))
    (check-exn #px"Bad arg types:\n\tassertion failed: " ;; arguments can not be mixed ...
               (λ () (typed->indexed (declare-argument-types! (λ (a b c) 1) (list vector-field? 1form-field? vector-field?)) basis)))
    (check-exn #px"Bad arg types:\n\tassertion failed: " ;; ... and must end in vector-fields
               (λ () (typed->indexed (declare-argument-types! (λ (a b c) 1) (list vector-field? 1form-field?  1form-field?)) basis)))
    (skip ;;TODO: this should be an error - we can check arity
     (check-exn #px"Bad arg types:\n\tassertion failed: "
               (λ () (typed->indexed (declare-argument-types! G (list 1form-field?  1form-field? vector-field?)) basis)))))
   (test-case
    "indexed->typed"
    (define-coordinates (up x y) R2-rect)
    (define pt ((point R2-rect) #(x y)))
    (define basis (coordinate-system->basis R2-rect))
    (define (F idx) (apply g:ref (up x y) idx))
    (declare-index-types! F (list up))
    (check-simplified? (((indexed->typed F basis) (literal-1form-field 'f R2-rect)) pt)
                       '(+ (* x (f_0 (up x y))) (* y (f_1 (up x y)))))
    (check-exn #px"Args do not match indices:\n\tassertion failed: "
               (λ () (((indexed->typed F basis) (literal-vector-field 'v R2-rect)) pt)))

    (define (G idx) (apply g:ref (up (down 1 2) (down 3 4)) idx))
    (declare-index-types! G (list up down))
    (check-equal? (argument-types (indexed->typed G basis)) (list 1form-field? vector-field?))
    (check-simplified? (((indexed->typed G basis) (literal-1form-field 'f R2-rect)
                                                  (literal-vector-field 'v R2-rect))
                        pt)
                       '(+ (* 2 (f_0 (up x y)) (v^1 (up x y)))
                           (*   (f_0 (up x y)) (v^0 (up x y)))
                           (* 4 (f_1 (up x y)) (v^1 (up x y)))
                           (* 3 (f_1 (up x y)) (v^0 (up x y)))))

    (check-exn #px"Bad index types:\n\tassertion failed: " ;; must have indexes
               (λ () (indexed->typed (λ (x) x) basis)))
    (check-exn #px"Bad index types:\n\tassertion failed: " ;; indexes must be up or down
               (λ () (indexed->typed (declare-index-types! G (list number? procedure?)) basis)))
    (check-exn #px"Bad index types:\n\tassertion failed: " ;; indexes can not be mixed ...
               (λ () (indexed->typed (declare-index-types! (λ (a b c) 1) (list down up down)) basis)))
    (check-exn #px"Bad index types:\n\tassertion failed: " ;; ... and must end in down
               (λ () (indexed->typed (declare-index-types! (λ (a b c) 1) (list down down up)) basis)))
    (skip ;;TODO: this should be an error - we can check arity
     (check-exn #px"Bad index types:\n\tassertion failed: "
                (λ () (indexed->typed (declare-index-types! G (list up up down)) basis)))))
   
   (test-case
    "count-occurrences"
    (check-equal? (count-occurrences up (list)) 0)
    (check-equal? (count-occurrences up (list up)) 1)
    (check-equal? (count-occurrences up (list down)) 0)
    (check-equal? (count-occurrences up (list up down up)) 2))
   (test-case
    "i:outer-product"
    (check-exn #px"T1 not index typed:\n\tassertion failed: i1"
               (λ () (i:outer-product (λ (x) x) (declare-index-types! (λ (x) x) (list up)))))
    (check-exn #px"T2 not index typed:\n\tassertion failed: i2"
               (λ () (i:outer-product (declare-index-types! (λ (x) x) (list up)) (λ (x) x))))
    (define P (i:outer-product (declare-index-types! (λ (x) x) (list up))
                               (declare-index-types! (λ (x) x) (list down))))
    (check-equal? (index-types P) (list up down))
    (check-exn #px"Wrong number of args to i:outer-product:\n\tassertion failed: "
               (λ () (P '(only-one)))))
   (test-case
    "list-with-inserted-coord"
    (check-equal? (list-with-inserted-coord '() 0 'a) '(a))
    (check-equal? (list-with-inserted-coord '(1 2 3) 0 'a) '(a 1 2 3))
    (check-equal? (list-with-inserted-coord '(1 2 3) 1 'a) '(1 a 2 3)))
   (test-case
    "i:contract"
    (check-exn #px"T not index typed:\n\tassertion failed: i-types"
               (λ () (i:contract (λ (x) x) 0 0 2)))
    (check-exn #px"Contraction indices not in range:\n\tassertion failed: "
               (λ () (i:contract (declare-index-types! (λ (x) x) (list up)) 0 0 2)))
    (check-equal? (index-types (i:contract (declare-index-types! (λ (x y) x) (list up down)) 0 0 0))
                  '())
    (check-equal? (index-types (i:contract (declare-index-types! (λ (w1 w2 w3 v1 v2) 1) (list up up up down down)) 1 0 1))
                  (list up up down)))
   
   (test-case
    "typed->structure"
    (define-coordinates (up x y) R2-rect)
    (define basis (coordinate-system->basis R2-rect))
    (define pt ((point R2-rect) #(x y)))

    ;; zero arguments is valid, but is it usefull?
    (define H (declare-argument-types! (λ () (λ (pt) pt)) '()))
    (check-equal? ((typed->structure H basis) pt) pt)
    
    (define (F v) (+ (* 2 (dy v)) (dx v)))
    (declare-argument-types! F (list vector-field?))
    (check-equal? ((typed->structure F basis) pt) (down 1 2))

    (define (G w v) (+ (* 2 (w d/dy)) (dx v)))
    (declare-argument-types! G (list 1form-field? vector-field?))
    (check-equal? ((typed->structure G basis) pt) (up (down 1 0) (down 3 2)))

    (check-exn #px"car: contract violation" ;;TODO: this error should be better
               (λ () (typed->structure (λ (x) x) basis)))
    (check-exn #px"Bad arg-type" ;; arguments must be vector-field? or 1form-field
               (λ () (typed->structure (declare-argument-types! G (list number? procedure?)) basis))))
   (test-case
    "structure->typed"
    (define-coordinates (up x y) R2-rect)
    (define basis (coordinate-system->basis R2-rect))
    (define pt ((point R2-rect) #(x y)))

    (define H (declare-argument-types! (λ () (λ (pt) pt)) '()))
    (check-equal? (((structure->typed (typed->structure H basis) basis)) pt) ((H) pt))

    (define (F v) (+ (* 2 (dy v)) (dx v)))
    (declare-argument-types! F (list vector-field?))
    (define V (literal-vector-field 'v R2-rect))
    (check-simplified? (((structure->typed (typed->structure F basis) basis) V) pt)
                       ((F V) pt))

    (define (G w v) (+ (* 2 (w d/dy) (dx v))
                       (* 3 (w d/dx) (dy v))))
    ;;TODO;; this only works if G is a sum of terms in all w's and v's
    ;; versions that don't work: (+ (* 2 (w d/dy) (dx v)) (dy v))
    ;;                           (+ (* 2 (w d/dy)) (dx v))
    ;;                           (sin (* (w d/dy) (dx v)))
    ;; if these are not valid, should they raise an error?
    ;; is this an older attempt that now is replaced with index-types?
    (declare-argument-types! G (list 1form-field? vector-field?))
    (define 1F (literal-1form-field '1f R2-rect))
    (check-simplified? (((structure->typed (typed->structure G basis) basis) 1F V) pt)
                       ((G 1F V) pt))
    
    (check-exn #px"assertion failed: \\(fix:= \\(length args\\) \\(length arg-types\\)\\)"
               (λ () ((structure->typed (typed->structure G basis) basis) 1F)))
    (check-exn #px"assertion failed: \\(arg-type arg\\)"
               (λ () ((structure->typed (typed->structure G basis) basis) V 1F)))
    )

   (test-case
    "maybe-simplify-coeff-functions"
    (check-equal? ((simplify-coeff-function 'a) 'b) 'b)
    (define R2-basis (coordinate-system->basis R2-rect))
    (define g (gensym))
    (define h (gensym))
    (set-simplify-coeff-function! #f)
    (check-equal? ((simplify-coeff-function 'a) 'b) 'b)
    (check-equal? (maybe-simplify-coeff-functions g R2-basis) g)
    
    (set-simplify-coeff-function! (λ (m) (λ (f) h)))
    (check-equal? ((simplify-coeff-function 'a) 'b) h)
    (check-equal? (maybe-simplify-coeff-functions g 'not-a-coordinate-basis) g)
    (check-equal? (maybe-simplify-coeff-functions g R2-basis) h)
    (set-simplify-coeff-function! #f)
    (check-exn #px"set-simplify-coeff-functions!: not a valid simplifier: "
               (λ () (set-simplify-coeff-function! 'wrong)))
    )

   (test-case
    "(zero/one-)manifold-function?"
    (check-true (zero-manifold-function? zero-manifold-function))
    (check-false (zero-manifold-function? one-manifold-function))
    (check-true (one-manifold-function? one-manifold-function))
    (check-false (one-manifold-function? zero-manifold-function))
    (check-true (manifold-function-cofunction? (λ (x) x)))
    (check-true (manifold-function-cofunction? 1))
    (check-true (manifold-function-cofunction? 'a))
    (check-false (manifold-function-cofunction? (down 'a)))
    (check-equal? (+ zero-manifold-function 'a) 'a)
    (check-equal? (+ 'a zero-manifold-function) 'a)
    (check-equal? (* zero-manifold-function 'a) zero-manifold-function)
    (check-equal? (* 'a zero-manifold-function) zero-manifold-function)
    (check-equal? (* one-manifold-function 'a) 'a)
    (check-equal? (* 'a one-manifold-function) 'a))
   
   (test-case
    "indexed->typed / typed->indexed"
    (define-coordinates (up x y) R2-rect)
    (define (T w1 w2 v1)
      (+ (* 'a (dx v1) (w1 d/dx) (w2 d/dy))
         (* 'b (dy v1) (w1 d/dy) (w2 d/dx))
         (* 'c (dy v1) (w1 d/dy) (w2 d/dy))))
    (declare-argument-types! T (list 1form-field? 1form-field? vector-field?))
    (check-simplified? (((indexed->typed
                          (typed->indexed T (coordinate-system->basis R2-rect))
                          (coordinate-system->basis R2-rect))
                         (literal-1form-field 'w1 R2-rect)
                         (literal-1form-field 'w2 R2-rect)
                         (literal-vector-field 'v1 R2-rect))
                        ((point R2-rect) (up 'x 'y)))
                       '(+ (* a (w2_1 (up x y)) (w1_0 (up x y)) (v1^0 (up x y)))
                           (* b (w2_0 (up x y)) (w1_1 (up x y)) (v1^1 (up x y)))
                           (* c (w2_1 (up x y)) (w1_1 (up x y)) (v1^1 (up x y))))))
   (test-case
    "i:outer-product / i:contract"
    (define-coordinates (up x y) R2-rect)
    (define (T1 w1 w2 v1)
      (+ (* 'a (dx v1) (w1 d/dx) (w2 d/dy))
         (* 'b (dy v1) (w1 d/dy) (w2 d/dx))
         (* 'c (dy v1) (w1 d/dy) (w2 d/dy))))
    (declare-argument-types! T1 (list 1form-field? 1form-field? vector-field?))
    (define iT1 (typed->indexed T1 (coordinate-system->basis R2-rect)))
    (define (T2 w1 w2)
      (+ (* (w1 d/dx) (w2 d/dx))
         (* (w1 d/dy) (w2 d/dy))
         (* (w1 d/dy) (w2 d/dx))))
    (declare-argument-types! T2 (list 1form-field? 1form-field?))
    (define iT2  (typed->indexed T2 (coordinate-system->basis R2-rect)))
    (define iT3 (i:outer-product iT1 iT2))
    (check-simplified? (((indexed->typed iT3 (coordinate-system->basis R2-rect))
                         (literal-1form-field 'w1 R2-rect)
                         (literal-1form-field 'w2 R2-rect)
                         (literal-1form-field 'w3 R2-rect)
                         (literal-1form-field 'w4 R2-rect)
                         (literal-vector-field 'v1 R2-rect))
                        ((point R2-rect) (up 'x 'y)))
                       '(+ (* a (w1_0 (up x y)) (v1^0 (up x y)) (w2_1 (up x y)) (w3_0 (up x y)) (w4_0 (up x y)))
                           (* a (w1_0 (up x y)) (v1^0 (up x y)) (w2_1 (up x y)) (w4_1 (up x y)) (w3_1 (up x y)))
                           (* a (w1_0 (up x y)) (v1^0 (up x y)) (w2_1 (up x y)) (w4_0 (up x y)) (w3_1 (up x y)))
                           (* b (w2_0 (up x y)) (w1_1 (up x y)) (v1^1 (up x y)) (w3_0 (up x y)) (w4_0 (up x y)))
                           (* b (w2_0 (up x y)) (w1_1 (up x y)) (v1^1 (up x y)) (w4_1 (up x y)) (w3_1 (up x y)))
                           (* b (w2_0 (up x y)) (w1_1 (up x y)) (v1^1 (up x y)) (w4_0 (up x y)) (w3_1 (up x y)))
                           (* c (w2_1 (up x y)) (w1_1 (up x y)) (v1^1 (up x y)) (w3_0 (up x y)) (w4_0 (up x y)))
                           (* c (w2_1 (up x y)) (w1_1 (up x y)) (v1^1 (up x y)) (w4_1 (up x y)) (w3_1 (up x y)))
                           (* c (w2_1 (up x y)) (w1_1 (up x y)) (v1^1 (up x y)) (w4_0 (up x y)) (w3_1 (up x y)))))
    (check-simplified? (((indexed->typed (i:contract iT1 0 0 2)
                                         (coordinate-system->basis R2-rect))
                         (literal-1form-field 'w1 R2-rect))
                        ((point R2-rect) (up 'x 'y)))
                       '(+ (* a (w1_1 (up x y)))
                           (* b (w1_0 (up x y)))
                           (* c (w1_1 (up x y)))))
    (check-simplified? (((indexed->typed (i:contract iT1 1 0 2)
                                         (coordinate-system->basis R2-rect))
                         (literal-1form-field 'w1 R2-rect))
                        ((point R2-rect) (up 'x 'y)))
                       '(* c (w1_1 (up x y))))
    (check-simplified? (((indexed->typed (i:contract iT3 1 0 0)
                                         (coordinate-system->basis R2-rect))
                         (literal-1form-field 'w1 R2-rect)
                         (literal-1form-field 'w2 R2-rect)
                         (literal-1form-field 'w3 R2-rect))
                        ((point R2-rect) (up 'x 'y)))
                       0))
   (test-case
    "typed<->structure"
    (define-coordinates (up x y) R2-rect)
    (define (T v1 w1 w2)
      (+ (* 'a (dx v1) (w1 d/dx) (w2 d/dy))
         (* 'b (dy v1) (w1 d/dy) (w2 d/dx))
         (* 'c (dy v1) (w1 d/dy) (w2 d/dy))))
    (declare-argument-types! T (list vector-field? 1form-field? 1form-field?))
    (check-simplified? ((typed->structure T (coordinate-system->basis R2-rect))
                        ((point R2-rect) (up 'x 'y)))
                       '(down (up (up 0 a) (up 0 0)) (up (up 0 0) (up b c))))
    ;;; Outer index is first argument.  Inner index is last argument.
    (check-simplified? (((structure->typed
                          (typed->structure T (coordinate-system->basis R2-rect))
                          (coordinate-system->basis R2-rect))
                         (literal-vector-field 'v1 R2-rect)
                         (literal-1form-field 'w1 R2-rect)
                         (literal-1form-field 'w2 R2-rect))
                        ((point R2-rect) (up 'x 'y)))
                       '(+ (* a (w2_1 (up x y)) (w1_0 (up x y)) (v1^0 (up x y)))
                           (* b (w2_0 (up x y)) (w1_1 (up x y)) (v1^1 (up x y)))
                           (* c (w2_1 (up x y)) (w1_1 (up x y)) (v1^1 (up x y)))))

    (check-simplified? ((typed->structure
                         (structure->typed
                          (typed->structure T (coordinate-system->basis R2-rect))
                          (coordinate-system->basis R2-rect))
                         (coordinate-system->basis R2-rect))
                        ((point R2-rect) (up 'x 'y)))
                       '(down (up (up 0 a) (up 0 0)) (up (up 0 0) (up b c)))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))