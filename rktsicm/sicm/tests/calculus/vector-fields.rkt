#lang racket/base

(require rackunit
         "../../main.rkt"
         "../helper.rkt"
         )

(define tests
  (test-suite
   "calculus/vector-fields"
   (test-case
    "R3-rect"
    (define-coordinates (up x y z) R3-rect)
    (check-simplified? (((* (expt d/dy 2) x y d/dx) (* (sin x) (cos y)))
                        ((R3-rect '->point)(up 'a 'b 'c)))
                       '(+ (* -1 a b (cos a) (cos b)) (* -2 a (sin b) (cos a))))

    (define counter-clockwise (- (* x d/dy) (* y d/dx)))
    (define outward (+ (* x d/dx) (* y d/dy)))
    (define mr ((R3-rect '->point) (up 'x0 'y0 'z0)))

    (check-simplified? ((counter-clockwise (sqrt (+ (square x) (square y)))) mr)
                       0)
    (check-simplified? ((counter-clockwise (* x y)) mr)
                       '(+ (expt x0 2) (* -1 (expt y0 2))))
    (check-simplified? ((outward (* x y)) mr) '(* 2 x0 y0)))
   (test-case
    "cylindrical >>> failing"
    (define-coordinates (up x y z) R3-rect)
    (define-coordinates (up r theta zeta) R3-cyl)
    (define A (+ (* 'A_r d/dr) (* 'A_theta d/dtheta) (* 'A_z d/dzeta)))

    (check-simplified? ((vector-field->components A R3-rect) (up 'x 'y 'z))
                       '(up (+ (* -1 A_theta y) (/ (* A_r x) (sqrt (+ (expt x 2) (expt y 2)))))
                            (+ (* A_theta x) (/ (* A_r y) (sqrt (+ (expt x 2) (expt y 2)))))
                            A_z))
    (check-simplified? ((d/dtheta (up x y z))
                        ((R3-rect '->point) (up 'x 'y 'z)))
                       '(up (* -1 y) x 0))
    (check-simplified? ((d/dr (up x y z))
                        ((R3-rect '->point) (up 'x 'y 'z)))
                       '(up (/ x (sqrt (+ (expt x 2) (expt y 2))))
                            (/ y (sqrt (+ (expt x 2) (expt y 2))))
                            0))
    (check-simplified? ((d/dzeta (up x y z))
                        ((R3-rect '->point) (up 'x 'y 'z)))
                       '(up 0 0 1))

    (define e-theta (* (/ 1 r) d/dtheta))
    (define e-r d/dr)
    (define e-z d/dzeta)
    (define B (+ (* 'B_r e-r) (* 'B_theta e-theta) (* 'B_z e-z)))
    
    (check-simplified? ((vector-field->components B R3-rect) (up 'x 'y 'z))
                       '(up
                         (+ (/ (* B_r x) (sqrt (+ (expt x 2) (expt y 2))))
                            (/ (* -1 B_theta y) (sqrt (+ (expt x 2) (expt y 2)))))
                         (+ (/ (* B_r y) (sqrt (+ (expt x 2) (expt y 2))))
                            (/ (* B_theta x) (sqrt (+ (expt x 2) (expt y 2)))))
                         B_z))
    (check-simplified? ((vector-field->components d/dy R3-rect)
                        (up 'x0 'y0 'z0))
                       '(up 0 1 0))
    (check-simplified? ((vector-field->components d/dy R3-rect)
                        (up 'r0 'theta0 'z0))
                       '(up 0 1 0))
    (check-simplified? ((vector-field->components d/dy R3-cyl)
                        (up 1 pi/2 0))
                       '(up 1. 6.123031769111886e-17 0))
    (check-simplified? ((vector-field->components d/dy R3-cyl)
                        (up 1 0 0))
                       '(up 0 1 0))
    (check-simplified? ((vector-field->components d/dy R3-cyl)
                        (up 'r0 'theta0 'z))
                       '(up (sin theta0) (/ (cos theta0) r0) 0))

    (define R3-point ((R3-rect '->point) (up 'x0 'y0 'z0)))
    (check-simplified? (series:print
                        (((exp (* x d/dy))
                          (literal-function 'f (-> (UP Real Real Real) Real)))
                         R3-point)
                        4)
                       1)
    (check-simplified? (((coordinatize (literal-vector-field 'v R3-rect) R3-rect)
                         (literal-function 'f (-> (UP Real Real Real) Real)))
                        (up 'x0 'y0 'z0))
                       '(+ (* (((partial 0) f) (up x0 y0 z0)) (v^0 (up x0 y0 z0)))
                           (* (((partial 1) f) (up x0 y0 z0)) (v^1 (up x0 y0 z0)))
                           (* (((partial 2) f) (up x0 y0 z0)) (v^2 (up x0 y0 z0)))))

    (define circular (- (* x d/dy) (* y d/dx)))
    (check-simplified? (series:for-each simplify
                                        (((exp (coordinatize (* 'a circular) R3-rect))
                                          identity)
                                         (up 1 0 0))
                                        6)
                       '((up 1 0 0)
                         (up 0 a 0)
                         (up (* -1/2 (expt a 2)) 0 0)
                         (up 0 (* -1/6 (expt a 3)) 0)
                         (up (* 1/24 (expt a 4)) 0 0)
                         (up 0 (* 1/120 (expt a 5)) 0))))
   (test-case
    "R2-rect >>> failing"
    (define-coordinates (up x y) R2-rect)
    (define circular (- (* x d/dy) (* y d/dx)))
    (check-simplified? ((((evolution 6) 'a circular) (R2-rect '->coords))
                        ((R2-rect '->point) (up 1 0)))
                       '(up (+ (* -1/720 (expt a 6))
                               (* 1/24 (expt a 4))
                               (* -1/2 (expt a 2))
                               1)
                            (+ (* 1/120 (expt a 5))
                               (* -1/6 (expt a 3))
                               a)
                            0)))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests tests))