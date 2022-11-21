#lang racket/base

(require rackunit
         "../../main.rkt"
         "../helper.rkt"
         )

(define the-tests
  (test-suite
   "calculus/wedge"
   (test-case
    "R3-rect"
    (define-coordinates (up x y z) R3-rect)
    (define R3-point ((R3-rect '->point) (up 'x0 'y0 'z0)))
    (define w (literal-1form-field 'w R3-rect))
    (define u (literal-1form-field 'u R3-rect))
    (define v (literal-1form-field 'v R3-rect))
    (define X (literal-vector-field 'X R3-rect))
    (define Y (literal-vector-field 'Y R3-rect))
    (define Z (literal-vector-field 'Z R3-rect))
    (define W (literal-vector-field 'W R3-rect))
    ;;; Just checking that everything is working...
    (check-simplified? ((w X) R3-point)
                       '(+ (* (X^0 (up x0 y0 z0)) (w_0 (up x0 y0 z0)))
                           (* (X^1 (up x0 y0 z0)) (w_1 (up x0 y0 z0)))
                           (* (X^2 (up x0 y0 z0)) (w_2 (up x0 y0 z0)))))
    ;;; A few theorems
    (check-simplified? (((- (wedge (wedge w u) v) (wedge w (wedge u v))) X Y Z)
                        R3-point)
                       0)
    (check-simplified? (((- (wedge (+ w u) v) (+ (wedge w v) (wedge u v))) X Y)
                        R3-point)
                       0)
    ;;; Note, a product of forms is their wedge!
    (check-simplified? (((- (wedge u v) (* u v)) X Y)
                        R3-point)
                       0))
   (test-case
    "dx^dy"
    (define-coordinates (up x y z) R3-rect)
    (define R3-point ((R3-rect '->point) (up 'x0 'y0 'z0)))
    (define dx^dy (wedge dx dy))
    (check-simplified? ((dx^dy d/dx d/dy) R3-point) 1)
    (check-simplified? ((dx^dy d/dx d/dx) R3-point) 0)
    (check-simplified? ((dx^dy d/dy d/dx) R3-point) -1))

   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))