#lang racket/base

(require rackunit
         "../../main.rkt"
         )

(define calculus/basis
  (test-suite
   "calculus/basis"
   (test-case "ORIG:vector-basis->dual"
              (let ()
                (install-coordinates S2-spherical (up 'theta 'phi))

                (define e0
                  (components->vector-field
                   (up (literal-function 'e0t (-> (UP* Real) Real))
                       (literal-function 'e0p (-> (UP* Real) Real)))
                   S2-spherical))

                (define e1
                  (components->vector-field
                   (up (literal-function 'e1t (-> (UP* Real) Real))
                       (literal-function 'e1p (-> (UP* Real) Real)))
                   S2-spherical))

                (define edual
                  (vector-basis->dual (down e0 e1) S2-spherical))

                (check-equal? (simplify
                               ((edual (down e0 e1))
                                ((S2-spherical '->point)
                                 (up 'theta0 'phi0))))
                              '(up (down 1 0) (down 0 1)))))

   (test-case "ORIG:Jacobian"
              (let ()
                (define v (literal-vector-field 'v R2-rect))

                (define vjp
                  (* (Jacobian (R2-polar 'coordinate-basis)
                               (R2-rect 'coordinate-basis))
                     ((R2-rect 'coordinate-basis-1form-fields)
                      v)))

                (check-equal? (simplify (vjp ((R2-rect '->point) (up 'x 'y))))
                              '(up
                                (/ (+ (* x (v^0 (up x y))) (* y (v^1 (up x y))))
                                   (sqrt (+ (expt x 2) (expt y 2))))
                                (/ (+ (* x (v^1 (up x y))) (* -1 y (v^0 (up x y))))
                                   (+ (expt x 2) (expt y 2)))))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests calculus/basis))