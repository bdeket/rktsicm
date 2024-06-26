#lang racket/base

(require rackunit
         "../../main.rkt"
         "../helper.rkt"
         )

(void (clear-arguments)
      (suppress-arguments (list '(up x0 y0 z0))))
(provide the-tests)
(define the-tests
  (test-suite
   "calculus/exterior-derivative"
;;; This is an excessively complicated program.  Another, more
;;; elementary program would, for a k-form, extract the cofficient
;;; functions relative to a literal basis, by applying it to the basis
;;; vectors, do the derivatives of the coefficients, to make one
;;; forms, and form the sum of the weges of the new 1-forms with the
;;; wedges of the corresponding dual basis elements.
   (test-case
    "first"
    (define-coordinates (up x y z) R3-rect)
    (define R3-rect-point ((R3-rect '->point) (up 'x0 'y0 'z0)))
    (define-coordinates (up r theta zeta) R3-cyl)
    (define R3-cyl-point ((R3-cyl '->point) (up 'r0 'theta0 'zeta0)))
    (define w (literal-1form-field 'w R3-rect))
    (define u (literal-1form-field 'u R3-rect))
    (define v (literal-1form-field 'v R3-rect))
    (define X (literal-vector-field 'X R3-rect))
    (define Y (literal-vector-field 'Y R3-rect))
    (define Z (literal-vector-field 'Z R3-rect))
    (define W (literal-vector-field 'W R3-rect))
    (check-simplified? (((d (literal-scalar-field 'f R3-rect)) X)
                        R3-rect-point)
                       '(+ (* (((partial 0) f) (up x0 y0 z0)) (X^0 (up x0 y0 z0)))
                           (* (((partial 1) f) (up x0 y0 z0)) (X^1 (up x0 y0 z0)))
                           (* (((partial 2) f) (up x0 y0 z0)) (X^2 (up x0 y0 z0)))))
    (check-simplified? ((((square d) (literal-scalar-field 'f R3-rect)) X Y)
                        R3-cyl-point)
                       0)
    ;;; To aid reading of expressions...
    (check-simplified? (((d w) X Y) R3-rect-point)
                       '(+ (* Y^2 ((partial 0) w_2) X^0)
                           (* Y^2 ((partial 1) w_2) X^1)
                           (* -1 Y^2 ((partial 2) w_0) X^0)
                           (* -1 Y^2 ((partial 2) w_1) X^1)
                           (* -1 Y^0 ((partial 0) w_2) X^2)
                           (* Y^0 ((partial 2) w_0) X^2)
                           (* Y^0 ((partial 1) w_0) X^1)
                           (* -1 Y^0 ((partial 0) w_1) X^1)
                           (* -1 ((partial 1) w_2) Y^1 X^2)
                           (* ((partial 2) w_1) Y^1 X^2)
                           (* -1 Y^1 ((partial 1) w_0) X^0)
                           (* Y^1 ((partial 0) w_1) X^0)))
    (define omega
      (+ (* (literal-scalar-field 'omega_0 R3-rect)
            (wedge dx dy))
         (* (literal-scalar-field 'omega_1 R3-rect)
            (wedge dy dz))
         (* (literal-scalar-field 'omega_2 R3-rect)
            (wedge dz dx))))
    (check-simplified? (((d omega) X Y Z) R3-rect-point)
                       '(+ (* X^0 Z^2 ((partial 0) omega_1) Y^1)
                           (* X^0 Z^2 ((partial 1) omega_2) Y^1)
                           (* X^0 Z^2 ((partial 2) omega_0) Y^1)
                           (* -1 X^0 Y^2 Z^1 ((partial 0) omega_1))
                           (* -1 X^0 Y^2 Z^1 ((partial 1) omega_2))
                           (* -1 X^0 Y^2 Z^1 ((partial 2) omega_0))
                           (* -1 Z^2 X^1 Y^0 ((partial 0) omega_1))
                           (* -1 Z^2 X^1 Y^0 ((partial 1) omega_2))
                           (* -1 Z^2 X^1 Y^0 ((partial 2) omega_0))
                           (* X^1 Y^2 Z^0 ((partial 0) omega_1))
                           (* X^1 Y^2 Z^0 ((partial 1) omega_2))
                           (* X^1 Y^2 Z^0 ((partial 2) omega_0))
                           (* X^2 Y^0 Z^1 ((partial 0) omega_1))
                           (* X^2 Y^0 Z^1 ((partial 1) omega_2))
                           (* X^2 Y^0 Z^1 ((partial 2) omega_0))
                           (* -1 X^2 Z^0 ((partial 0) omega_1) Y^1)
                           (* -1 X^2 Z^0 ((partial 1) omega_2) Y^1)
                           (* -1 X^2 Z^0 ((partial 2) omega_0) Y^1)))
    (check-simplified? (((d (d omega)) X Y Z W) R3-rect-point) 0))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))