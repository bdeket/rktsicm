#lang racket/base

(require rackunit
         "../../main.rkt"
         "../helper.rkt"
         )

(define kernel-tests
  (test-suite
   "calculus/form-field"
   (test-case "rectangular"
              (install-coordinates R3-rect (up 'x 'y 'z))

              (define mr ((R3-rect '->point) (up 'x0 'y0 'z0)))

              (define a-1form
                (components->1form-field
                 (down (literal-function 'ax (-> (UP* Real) Real))
                       (literal-function 'ay (-> (UP* Real) Real))
                       (literal-function 'az (-> (UP* Real) Real)))
                 R3-rect))

              (define a-vector-field
                (components->vector-field
                 (up (literal-function 'vx (-> (UP* Real) Real))
                     (literal-function 'vy (-> (UP* Real) Real))
                     (literal-function 'vz (-> (UP* Real) Real)))
                 R3-rect))

              (check-simplified? ((a-1form a-vector-field) mr)
                                 '(+ (* (vx (up x0 y0 z0)) (ax (up x0 y0 z0)))
                                     (* (vy (up x0 y0 z0)) (ay (up x0 y0 z0)))
                                     (* (vz (up x0 y0 z0)) (az (up x0 y0 z0)))))

              (check-simplified? ((1form-field->components a-1form R3-rect) (up 'x0 'y0 'z0))
                                 '(down (ax (up x0 y0 z0)) (ay (up x0 y0 z0)) (az (up x0 y0 z0))))


              (install-coordinates R3-cyl (up 'r 'theta 'zeta))
              (check-simplified? ((1form-field->components a-1form R3-cyl)
                                  (up 'r0 'theta0 'z0))
                                 '(down
                                   (+ (* (cos theta0)
                                         (ax (up (* r0 (cos theta0)) (* r0 (sin theta0)) z0)))
                                      (* (sin theta0)
                                         (ay (up (* r0 (cos theta0)) (* r0 (sin theta0)) z0))))
                                   (+ (* r0 (cos theta0)
                                         (ay (up (* r0 (cos theta0)) (* r0 (sin theta0)) z0)))
                                      (* -1 r0 (sin theta0)
                                         (ax (up (* r0 (cos theta0)) (* r0 (sin theta0)) z0))))
                                   (az (up (* r0 (cos theta0)) (* r0 (sin theta0)) z0))))
              )
   (test-case "cylindrical"
              (define-coordinates (up x y z) R3-rect)
              (define-coordinates (up r theta zeta) R3-cyl)
              (define mr ((R3-rect '->point) (up 'x0 'y0 'z0)))
              (define mp ((R3-cyl '->point) (up 'r0 'theta0 'z0)))

              (check-equal? ((dx d/dx) mr) 1)
              (check-equal? ((dx d/dx) mp) 1)
              (check-simplified? ((1form-field->components dr R3-rect) (up 'x0 'y0 'z0))
                                 '(down (/ x0 (sqrt (+ (expt x0 2) (expt y0 2))))
                                        (/ y0 (sqrt (+ (expt x0 2) (expt y0 2))))
                                        0))
              (check-simplified? ((1form-field->components dtheta R3-rect) (up 'x0 'y0 'z0))
                                 '(down (/ (* -1 y0) (+ (expt x0 2) (expt y0 2)))
                                        (/ x0 (+ (expt x0 2) (expt y0 2)))
                                        0))
              (check-simplified? (((+ (* 'w_0 dr) (* 'w_1 dtheta)) (+ (* 'V^0 d/dx) (* 'V^1 d/dy)))
                                  mp)
                                 '(+ (* V^0 w_0 (cos theta0))
                                     (* V^1 w_0 (sin theta0))
                                     (/ (* -1 V^0 w_1 (sin theta0)) r0)
                                     (/ (* V^1 w_1 (cos theta0)) r0)))
              (check-simplified? (((components->1form-field (1form-field->components
                                                             (+ (* 'w_0 dr) (* 'w_1 dtheta))
                                                             R3-rect)
                                                            R3-rect)
                                   (+ (* 'V^0 d/dx) (* 'V^1 d/dy)))
                                  mp)
                                 '(+ (* V^0 w_0 (cos theta0))
                                     (* V^1 w_0 (sin theta0))
                                     (/ (* -1 V^0 w_1 (sin theta0)) r0)
                                     (/ (* V^1 w_1 (cos theta0)) r0)))

              (define counter-clockwise (- (* x d/dy) (* y d/dx)))
              (define outward (+ (* x d/dx) (* y d/dy)))
              (check-simplified? ((dx counter-clockwise) mr) '(* -1 y0))
              (check-simplified? ((dx outward) mr) 'x0)
              (check-simplified? ((dr counter-clockwise) mp) 0)
              (check-simplified? ((dr outward) mp) 'r0)
              (check-simplified? ((dr outward) mr) '(sqrt (+ (expt x0 2) (expt y0 2))))
              (check-simplified? (((* x dy) (+ (* 'u d/dx) (* 'v d/dy))) mr) '(* v x0))
              (check-simplified? ((dr d/dr) ((R3-rect '->point) (up 'x^0 'y^0 'z^0))) 1)
              (check-simplified? ((dr d/dtheta) ((R3-rect '->point) (up 'x^0 'y^0 'z^0))) 0)
              (check-simplified? ((dtheta d/dr) ((R3-rect '->point) (up 'x^0 'y^0 'z^0))) 0)
              (check-simplified? ((dtheta d/dtheta) ((R3-rect '->point) (up 'x^0 'y^0 'z^0))) 1))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests kernel-tests))