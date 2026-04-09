#lang racket/base

(require rackunit
         "../../main.rkt"
         "../../general/eq-properties.rkt"
         "../helper+scm.rkt"
         )

(define (check-vf vf [name 'unnamed-vector-field]
                  #:fields [fields (list function?)]
                  #:proc [proc #f])
  (check-true (vector-field? vf))
  (check-true (operator? vf))
  (check-equal? (operator-subtype vf) 'vector-field)
  (check-equal? (operator-arity vf) *exactly-one*)
  (when name (check-equal? (expression (operator-name vf)) name))
  (when fields (check-equal? (eq-get vf 'argument-types) fields))
  (when proc (check-equal? (operator-procedure vf) proc)))
(provide the-tests)
(define the-tests
  (test-suite
   "calculus/vector-fields"
   (test-case
    "procedure->vector-field"
    (define vf (procedure->vector-field (λ (f) f)))
    (check-vf vf)
    (define vf2 (procedure->vector-field (λ (f) f) '∇))
    (check-vf vf2 '∇))
   (test-case
    "components->vector-field"
    (define vf
      (components->vector-field (up (literal-function 'x (-> (UP* Real 2) Real))
                                    (literal-function 'y (-> (UP* Real 2) Real)))
                                R2-rect))
    (check-vf vf '(vector-field (up x y)))
    (define vf2 (components->vector-field (up (λ (c) (+ (* 1/2 (ref c 0)) (* 3 (ref c 1))))
                                              (λ (c) (+ (* 2 (ref c 0)) (* 1/3 (ref c 1)))))
                                R2-rect 'linear))
    (check-vf vf2 'linear)
    ;; TODO - is this how it should be used?
    (check-equal? (expression ((vf2 (R2-rect '->coords)) ((R2-rect '->point) #(a b))))
                  '(up (+ (* 1/2 a) (* 3 b)) (+ (* 2 a) (* 1/3 b)))))
   (test-case
    "vector-field->components"
    (define vf (components->vector-field (up (λ (c) (+ (* 1/2 (ref c 0)) (* 3 (ref c 1))))
                                             (λ (c) (+ (* 2 (ref c 0)) (* 1/3 (ref c 1)))))
                                         R2-rect 'linear))
    (check-equal? (simplify ((vector-field->components vf R2-rect) #(a b)))
                  '(up (+ (* 1/2 a) (* 3 b)) (+ (* 2 a) (* 1/3 b))))
    (check-equal? (simplify ((vector-field->components vf R2-polar) #(r α)))
                  '(up (+ (* 1/6 r (expt (cos α) 2)) (* 5 r (cos α) (sin α)) (* 1/3 r))
                    (+ -3 (* 5 (expt (cos α) 2)) (* -1/6 (cos α) (sin α)))))
    (check-exn #px"Bad vector field: vector-field->components" (λ () (vector-field->components 'any R2-rect))))
   (test-case
    "zero"
    (define vf (components->vector-field (up (λ (c) c)) R1-rect 'linear))
    (check-equal? (vf:zero vf) zero-manifold-function)
    (define vf2 (vf:zero-like vf))
    (check-vf vf2 'vf:zero #:fields #f #:proc vf:zero)
    (check-true (vf:zero? vf2))
    (check-false (vf:zero? vf))
    (check-exn #px"vf:zero-like:\n\tassertion failed: " (λ () (vf:zero-like 'any)))
    (check-exn #px"vf:zero\\?:\n\tassertion failed:" (λ () (vf:zero? 'any))))
   (test-case
    "literal-vector-field"
    (define X (literal-vector-field 'X R1-rect))
    (check-vf X 'X)
    (check-simplified? ((X (R1-rect '->coords)) ((R1-rect '->point) 't))
                       '(up (X^0 t)))
    (check-simplified? (((literal-vector-field 'R R2-rect) (R2-rect '->coords)) ((R2-polar '->point) #(x y)))
                       '(up (R^0 (up (* x (cos y)) (* x (sin y))))
                            (R^1 (up (* x (cos y)) (* x (sin y)))))))
   (test-case
    "coordinate-basis-vector-field"
    (define vf (coordinate-basis-vector-field R1-rect 'base))
    (define vf2 (coordinate-basis-vector-field R2-polar 'base))
    (define vf2_1 (coordinate-basis-vector-field R2-polar 'base 1))
    (check-vf vf 'base)
    (check-simplified? ((vf (R1-rect '->coords)) ((R1-rect '->point) 't))
                       1)
    (check-simplified? ((vf2 (R2-polar '->coords)) ((R2-polar '->point) (up 'r 'α)))
                       (down (up 1 0) (up 0 1)))
    (check-simplified? ((vf2 (R2-rect '->coords)) ((R2-polar '->point) (up 'r 'α)))
                       '(down (up (cos α) (sin α)) (up (* -1 r (sin α)) (* r (cos α)))))
    (check-simplified? ((vf2_1 (R2-rect '->coords)) ((R2-polar '->point) (up 'r 'α)))
                       '(up (* -1 r (sin α)) (* r (cos α)))))
   (test-case
    "coordinate-system->vector-basis"
    (check-simplified? (coordinate-system->vector-basis R2-polar)
                       '(down d/dx0 d/dx1))
    (check-simplified? (coordinate-system->vector-basis R2-rect)
                       '(down d/dx0 d/dx1)))
   (test-case
    "basis-components<->vector-field"
    (define vf (basis-components->vector-field (up (compose (literal-function 'X (-> (UP* Real 2) Real))
                                                            (R2-rect '->coords))
                                                   (compose (literal-function 'Y (-> (UP* Real 2) Real))
                                                            (R2-rect '->coords)))
                                               (basis->vector-basis (coordinate-system->basis R2-rect))))
    (check-vf vf #f)
    (define-coordinates (up x y) R2-rect)
    (check-simplified? ((vf (R2-rect '->coords)) ((R2-rect '->point) #(x y)))
                       '(up (X (up x y)) (Y (up x y))))
    (check-simplified? (((vector-field->basis-components vf (up (λ (x) (ref x 0)) (λ (y) (ref y 1)))) (R2-rect '->coords))
                        ((R2-polar '->point) (up 'x 'y)))
                      '(up (X (up (* x (cos y)) (* x (sin y)))) (Y (up (* x (cos y)) (* x (sin y)))))))
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
    "cylindri"
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
                       '(up 1. 6.123233995736766e-17 0))
    (check-simplified? ((vector-field->components d/dy R3-cyl)
                        (up 1 0 0))
                       '(up 0 1 0))
    (check-simplified? ((vector-field->components d/dy R3-cyl)
                        (up 'r0 'theta0 'z))
                       '(up (sin theta0) (/ (cos theta0) r0) 0))

    (define R3-point ((R3-rect '->point) (up 'x0 'y0 'z0)))
    (check-exn #px"Wrong type argument -- LITERAL-FUNCTION"
               (λ ()(series:print
                     (((exp (* x d/dy))
                       (literal-function 'f (-> (UP Real Real Real) Real)))
                      R3-point)
                     4)))
    (check-simplified? (((coordinatize (literal-vector-field 'v R3-rect) R3-rect)
                         (literal-function 'f (-> (UP Real Real Real) Real)))
                        (up 'x0 'y0 'z0))
                       '(+ (* (((partial 0) f) (up x0 y0 z0)) (v^0 (up x0 y0 z0)))
                           (* (((partial 1) f) (up x0 y0 z0)) (v^1 (up x0 y0 z0)))
                           (* (((partial 2) f) (up x0 y0 z0)) (v^2 (up x0 y0 z0)))))

    (define circular (- (* x d/dy) (* y d/dx)))
    (check-simplified? (accumulate pp
                                   (series:for-each (λ (x) (pp (simplify x)))
                                                    (((exp (coordinatize (* 'a circular) R3-rect))
                                                      identity)
                                                     (up 1 0 0))
                                                    6))
                       '((up 1 0 0)
                         (up 0 a 0)
                         (up (* -1/2 (expt a 2)) 0 0)
                         (up 0 (* -1/6 (expt a 3)) 0)
                         (up (* 1/24 (expt a 4)) 0 0)
                         (up 0 (* 1/120 (expt a 5)) 0))))
   (test-case
    "R2-rect"
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
                               a))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))