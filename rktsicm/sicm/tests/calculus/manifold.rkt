#lang racket/base

(require rackunit
         "../../main.rkt"
         "../helper.rkt"
         )

(provide the-tests)
(define the-tests
  (test-suite
   "calculus/manifold"
   (test-case
    "R2-rect"
    (define m ((R2-rect '->point) (up 3 4)))
    (check-simplified? ((R2-polar '->coords) m)
                       #(5 .9272952180016122))
    (define-coordinates (up x y) R2-rect)
    (define-coordinates (up r theta) R2-polar)
    (define mr ((R2-rect '->point) (up 'x0 'y0)))
    (define mp ((R2-polar '->point) (up 'r0 'theta0)))
    (define circular (- (* x d/dy) (* y d/dx)))

    (check-simplified? ((circular (+ (* 2 x) (* 3 y))) mr)
                       '(+ (* 3 x0) (* -2 y0)))
    (check-simplified? ((circular theta) mr) 1)
    (check-simplified? ((dr circular) mr) 0)
    (check-simplified? (((d r) d/dr) mr) 1)
    (check-simplified? ((dr d/dr) mr) 1)
    (check-simplified? ((dr (literal-vector-field 'v R2-polar)) mr)
                       '(v^0 (up (sqrt (+ (expt x0 2) (expt y0 2))) (atan y0 x0))))
    (check-simplified? (((d r) (literal-vector-field 'v R2-polar)) mr)
                       '(v^0 (up (sqrt (+ (expt x0 2) (expt y0 2))) (atan y0 x0))))
    (check-simplified? ((dr (literal-vector-field 'v R2-rect)) mr)
                       '(+ (/ (* x0 (v^0 (up x0 y0))) (sqrt (+ (expt x0 2) (expt y0 2))))
                           (/ (* y0 (v^1 (up x0 y0))) (sqrt (+ (expt x0 2) (expt y0 2))))))
    (check-simplified? (((d r) (literal-vector-field 'v R2-rect)) mr)
                       '(+ (/ (* x0 (v^0 (up x0 y0))) (sqrt (+ (expt x0 2) (expt y0 2))))
                           (/ (* y0 (v^1 (up x0 y0))) (sqrt (+ (expt x0 2) (expt y0 2))))))
    ;;; Consider the two following metrics on the space
    (define (g-polar u v)
      (+ (* (dr u) (dr v))
         (* (* r (dtheta u)) (* r (dtheta v)))))
    (define (g-rect u v)
      (+ (* (dx u) (dx v))
         (* (dy u) (dy v))))
    (check-simplified? (((- g-polar g-rect)
                         (literal-vector-field 'v R2-rect)
                         (literal-vector-field 'v R2-rect))
                        mr)
                       0)
    (check-simplified? (((- g-polar g-rect)
                         (literal-vector-field 'v R2-polar)
                         (literal-vector-field 'v R2-polar))
                        mr)
                       0)
    (check-simplified? (((- g-polar g-rect)
                         (literal-vector-field 'v R2-polar)
                         (literal-vector-field 'v R2-polar))
                        mp)
                       0)
    (check-simplified? (((- g-polar g-rect)
                         (literal-vector-field 'v R2-rect)
                         (literal-vector-field 'v R2-rect))
                        mp)
                       0)
    )
   (test-case
    "S2-shperical"
    (define m ((S2-spherical '->point) (up 'theta 'phi)))
    (check-simplified? ((S2-tilted '->coords) m)
                       '(up (acos (* -1 (sin phi) (sin theta)))
                            (atan (cos theta) (* (sin theta) (cos phi)))))
    )
   (test-case
    "S1-circular"
    (define m ((S1-circular '->point) 'theta))
    (check-simplified? (manifold-point-representation m)
                       '(up (cos theta) (sin theta)))
    (check-simplified? ((compose (S1-circular '->coords) (S1-circular '->point)) 'theta)
                       'theta)
    (check-simplified? ((compose (S1-circular '->coords) (S1-tilted '->point)) 'theta)
                       '(atan (cos theta) (* -1 (sin theta)))))
   (test-case
    "S2p-spherical"
    (define m ((S2p-spherical '->point) (up 'theta 'phi)))
    (check-simplified? (manifold-point-representation m)
                       '(up (* (sin theta) (cos phi))
                            (* (sin phi) (sin theta))
                            (cos theta)))
    (check-simplified? ((compose (S2p-spherical '->coords) (S2p-spherical '->point))
                        (up 1. 0.))
                       '(up 1. 0.))
    (check-simplified? ((compose (S2p-spherical '->coords) (S2p-spherical '->point))
                        (up 0. 1.));; with 0 this is an error
                       '(up 0. 0.))
    ;Should be warned singular!

    (check-simplified? ((compose (S2p-spherical '->coords) (S2p-spherical '->point))
                        (up 'theta 'phi))
                       '(up theta phi))
    
    (check-simplified? ((compose (S2p-spherical '->coords) (S2p-tilted '->point))
                        (up 'theta 'phi))
                       '(up (atan (sqrt (+ (* (expt (sin theta) 2) (expt (cos phi) 2))
                                           (expt (cos theta) 2)))
                                  (* (sin phi) (sin theta)))
                            (atan (* -1 (cos theta))
                                  (* (sin theta) (cos phi))))))
   (test-case
    "S3-spherical"
    (check-simplified? ((compose (S3-spherical '->coords)
                                 (S3-spherical '->point))
                        (up 'a 'b 'c))
                       '(up a b c))
    (check-simplified? ((compose (S3-spherical '->coords)
                                 (S3-tilted '->point))
                        (up 'a 'b 'c))
                       '(up
                         (atan
                          (sqrt
                           (+ (* (expt (sin c) 2) (expt (sin b) 2) (expt (cos a) 2))
                              (* (expt (sin c) 2) (expt (cos b) 2))
                              (expt (cos c) 2)))
                          (* (sin c) (sin b) (sin a)))
                         (atan (sqrt (+ (* (expt (sin b) 2) (expt (sin a) 2) (expt (cos c) 2))
                                        (expt (cos a) 2)))
                               (* (sin a) (cos b)))
                         (atan (* -1 (cos a)) (* (sin b) (sin a) (cos c)))))
    (check-simplified? ((compose (S3-spherical '->coords)
                                 (S3-spherical '->point))
                        (up 0. 0. 0.)) ;with 0 this is an error
                       '(up 0. 0. 0.)))
   (test-case
    "S1"
    (define m ((S1-slope '->point) 's))
    (check-simplified? (manifold-point-representation m)
                       '(up (/ (* 2 s)
                               (+ 1 (expt s 2)))
                            (/ (+ -1 (expt s 2))
                               (+ 1 (expt s 2)))))
    (check-simplified? (manifold-point-representation
                        ((compose (S1-slope '->point)
                                  (S1-slope '->coords))
                         m))
                       '(up (/ (* 2 s)
                               (+ 1 (expt s 2)))
                            (/ (+ -1 (expt s 2))
                               (+ 1 (expt s 2)))))
    (check-simplified? ((compose (S1-slope '->coords)
                                 (S1-slope '->point))
                        's)
                       's))
   (test-case
    "S2p-Riemann"
    (define m ((S2p-Riemann '->point) (up 'x 'y)))
    (check-simplified? (manifold-point-representation m)
                       '(up (/ (* 2 x) 
                               (+ 1 (expt x 2) (expt y 2)))
                            (/ (* 2 y)
                               (+ 1 (expt y 2) (expt x 2)))
                            (/ (+ -1 (expt x 2) (expt y 2))
                               (+ +1 (expt x 2) (expt y 2)))))
    (check-simplified? (manifold-point-representation
                        ((compose (S2p-Riemann '->point) (S2p-Riemann '->coords))
                         m))
                       '(up (/ (* 2 x)
                               (+ 1 (expt x 2) (expt y 2)))
                            (/ (* 2 y)
                               (+ 1 (expt x 2) (expt y 2)))
                            (/ (+ -1 (expt x 2) (expt y 2))
                               (+ 1 (expt x 2) (expt y 2)))))
    (check-simplified? ((compose (S2p-Riemann '->coords) (S2p-Riemann '->point))
                        (up 'x 'y))
                       '(up x y))
    (check-simplified? (manifold-point-representation
                        ((S2p-Riemann '->point)
                         (up (cos 'theta) (sin 'theta))))
                       '(up (cos theta) (sin theta) 0)))
   (test-case
    "S1-gnomic"
    (define m ((S1-gnomic '->point) 's))
    (check-simplified? (manifold-point-representation m)
                       '(up (/ s (sqrt (+ 1 (expt s 2))))
                            (/ 1 (sqrt (+ 1 (expt s 2))))))
    (check-simplified? (manifold-point-representation
                        ((compose (S1-gnomic '->point) (S1-gnomic '->coords)) m))
                       '(up (/ s (sqrt (+ 1 (expt s 2))))
                            (/ 1 (sqrt (+ 1 (expt s 2))))))
    (check-simplified? ((compose (S1-slope '->coords) (S1-slope '->point)) 's)
                       's))
   (test-case
    "S2p-gnomic"
    (define m ((S2p-gnomic '->point) (up 'x 'y)))
    (check-simplified? (manifold-point-representation m)
                       '(up (/ x (sqrt (+ 1 (expt x 2) (expt y 2))))
                            (/ y (sqrt (+ 1 (expt x 2) (expt y 2))))
                            (/ 1 (sqrt (+ 1 (expt x 2) (expt y 2))))))
    (check-simplified? (manifold-point-representation
                        ((compose (S2p-gnomic '->point) (S2p-gnomic '->coords))
                         m))
                       '(up (/ x (sqrt (+ 1 (expt x 2) (expt y 2))))
                            (/ y (sqrt (+ 1 (expt x 2) (expt y 2))))
                            (/ 1 (sqrt (+ 1 (expt x 2) (expt y 2))))))
    (check-simplified? ((compose (S2p-gnomic '->coords) (S2p-gnomic '->point))
                        (up 'x 'y))
                       '(up x y))
    (check-simplified? (manifold-point-representation
                        ((S2p-gnomic '->point)
                         (up (cos 'theta) (sin 'theta))))
                       '(up (/ (cos theta) (sqrt 2))
                            (/ (sin theta) (sqrt 2)) 
                            (/ 1 (sqrt 2)))))
   (test-case
    "S2p-stereographic"
    (define q ((S2p-stereographic '->point) (up -3/2 3/2)))
    (define p ((S2p-stereographic '->point) (up 3/2 0)))
    (check-simplified? ((S2p-stereographic '->coords)
                        ((S2p-gnomic '->point)
                         (+ (* 't ((S2p-stereographic '->coords) p))
                            (* (- 1 't) ((S2p-stereographic '->coords) q)))))
                       '(up (/ (+ -15/10 (* 3 t))
                               (+ -1 (sqrt (+ 55/10 (* 1125/100 (expt t 2))
                                              (* -135/10 t)))))
                            (/ (+ 15/10 (* -15/10 t))
                               (+ -1 (sqrt (+ 55/10 (* 1125/100 (expt t 2))
                                              (* -135/10 t))))))))
   (test-case
     "S3"
     ;; Now a fun example synthesizing the to projective coordinates.
     ; S3 is one-to-one with the quaternions.
     ; We interpret the first three components of the embedding space as the
     ;    i,j,k imaginary party and the 4th component as the real part.
     ; The gnomic projection removes the double-cover of quaternions to rotations.
     ; The solid unit-sphere of the stereographic projection from the south pole likewise.
     (check-simplified? ((S3-gnomic '->coords) ((S3-stereographic '->point) (up 'x 'y 'z)))
                        '(up (/ (* 2 x) (+ -1 (expt x 2) (expt y 2) (expt z 2)))
                             (/ (* 2 y) (+ -1 (expt y 2) (expt x 2) (expt z 2)))
                             (/ (* 2 z) (+ -1 (expt z 2) (expt x 2) (expt y 2)))))
     (check-simplified? ((S3-stereographic '->coords) ((S3-gnomic '->point) (up 'x 'y 'z)))
                        '(up (/ x (+ -1 (sqrt (+ 1 (expt x 2) (expt y 2) (expt z 2)))))
                             (/ y (+ -1 (sqrt (+ 1 (expt y 2) (expt x 2) (expt z 2)))))
                             (/ z (+ -1 (sqrt (+ 1 (expt z 2) (expt x 2) (expt y 2)))))))
     
     (check-simplified? (euclidean-norm ((S3-stereographic '->coords)
                                         ((S3-gnomic '->point) (up 'x 'y 'z))))
                        '(/ (sqrt (+ (expt x 2) (expt y 2) (expt z 2)))
                            (sqrt (+ 2
                                     (expt x 2) (expt y 2) (expt z 2)
                                     (* -2
                                        (sqrt (+ 1 (expt x 2) (expt y 2) (expt z 2)))))))))
   (test-case
    "alternate-angles"
    (check-simplified? ((compose (alternate-angles '->coords)
                                 (Euler-angles '->point))
                        (up 'theta 'phi 'psi))
                       '(up
                         (asin (* (cos psi) (sin theta)))
                         (atan (+ (* (cos theta) (sin phi) (cos psi)) (* (cos phi) (sin psi)))
                               (+ (* (cos phi) (cos theta) (cos psi)) (* -1 (sin psi) (sin phi))))
                         (atan (* -1 (sin psi) (sin theta)) (cos theta))))
    (check-simplified? ((compose (Euler-angles '->coords)
                                 (alternate-angles '->point)
                                 (alternate-angles '->coords)
                                 (Euler-angles '->point))
                        (up 'theta 'phi 'psi))
                       '(up theta phi psi)))
   (test-case
    "scalar field"
    ;;; A scalar field can be defined by combining coordinate functions:
    (define-coordinates (up x y z) R3-rect)
    (define-coordinates (up r theta zeta) R3-cyl)
    (define h (+ 5 (square x) (* -1 x (cube y)) (/ 1 y)))
    ;;; The field, however defined, can be seen as independent of
    ;;; coordinate system:
    (check-simplified? (h ((R3-rect '->point) (up 3. 4. 'z))) -177.75)
    (check-simplified? (h ((R3-cyl '->point) (up 5. (atan 4 3) 'z))) -177.74999999999997)
    ;;; However this may be too clever, producing a traditional notation
    ;;; that is hard to understand deeply.  Perhaps it is better to be
    ;;; explicit about what is coordinate-system independent.  For
    ;;; example, we can define a coordinate-free function h by composing a
    ;;; definition in terms of coordinates with a coordinate function.
    (define (h-concrete xy)
      (let ((x (ref xy 0))
            (y (ref xy 1)))
        (+ 5
           (square x)
           (* -1 x (cube y))
           (/ 1 y))))
    (define h* (compose h-concrete (R3-rect '->coords)))
    (check-simplified? (h* ((R3-rect '->point) (up 3. 4 5))) -177.75))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))