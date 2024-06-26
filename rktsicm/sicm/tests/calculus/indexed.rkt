#lang racket/base

(require rackunit
         "../../main.rkt"
         "../helper.rkt"
         )

(provide the-tests)
(define the-tests
  (test-suite
   "calculus/indexed"
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