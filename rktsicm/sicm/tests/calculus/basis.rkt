#lang racket/base

(require rackunit
         "../../main.rkt"
         "../helper+scm.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "calculus/basis"
   (test-case
    "coordinate-basis"
    (define B (coordinate-system->basis R2-rect))
    (check-true (coordinate-basis? B))
    (check-equal? B (R2-rect 'coordinate-basis))
    (check-equal? (basis->coordinate-system B) R2-rect))
   (test-case
    "basis"
    (define B (make-basis 'vector-basis '1form-basis))
    (check-false (basis? 'any))
    (check-true (basis? (R2-rect 'coordinate-basis)))
    (check-true (basis? B))
    (check-equal? (basis->vector-basis B) 'vector-basis)
    (check-equal? (basis->1form-basis B) '1form-basis)
    (check-equal? (basis->dimension B) 1)
    (check-exn #px"assertion failed: \\(fix:= n " (λ () (make-basis 'vector-basis (up '1form 'basis))))
    (check-exn #px"Not a basis:\n\tassertion failed: " (λ () (basis->vector-basis 'any)))
    (check-exn #px"Not a basis:\n\tassertion failed: " (λ () (basis->1form-basis 'any)))
    (check-exn #px"Not a basis:\n\tassertion failed: " (λ () (basis->dimension 'any)))
    (check-exn #px"Not a coordinate basis:\n\tassertion failed: " (λ () (basis->coordinate-system B))))
   (test-case
    "contract"
    ;; TODO - I'm not understanding something ...
    ;; more extensively used in calculus/connection and calculus/metric
    (check-equal? ((contract (λ (vf 1f) (1f vf)) (R2-rect 'coordinate-basis))
                   ((R2-rect '->point) #(a b)))
                  2))
   (test-case
    "vector-basis->dual"
    (define vb (down (literal-vector-field 'd/dx  R2-rect)
                     (literal-vector-field 'd/dy R2-rect)))
    (define db (vector-basis->dual vb R2-rect))
    (check-true (form-field? (ref db 0)))
    (check-true (form-field? (ref db 1)))
    (check-simplified? ((db vb) ((R2-rect '->point) #(x y)))
                       '(up (down 1 0) (down 0 1)))
    (check-simplified? ((db vb) ((R2-polar '->point) #(r α)))
                       '(up (down 1 0) (down 0 1))))
   (test-case
    "constant-vector-field"
    (define-coordinates (up x y) R2-rect)
    (define vf (literal-vector-field 'S R2-rect))
    (define cvf (make-constant-vector-field (R2-rect 'coordinate-basis) ((R2-rect '->point) #(c d))))
    (check-simplified? (((cvf vf) (R2-rect '->coords)) ((R2-polar '->point) #(a b)))
                       '(up (S^0 (up c d)) (S^1 (up c d)))))
   (test-case "ORIG:vector-basis->dual"
              (let ()
                (install-coordinates S2-spherical (up 'theta 'phi))

                (define e0 ;; this is ±the same as (literal-vector-field 'e0 S2-spherical)
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
  (run-tests the-tests))