#lang racket/base

(require rackunit
         "../../main.rkt"
         "../helper.rkt"
         )

(define the-tests
  (test-suite
   "calculus/interior-product"
   (test-case
    "Claim L_x omega = i_x d omega + d i_x omega (Cartan Homotopy Formula)"
    (define-coordinates (up x y z) R3-rect)
    (define R3-rect-point ((R3-rect '->point) (up 'x0 'y0 'z0)))
    (define X (literal-vector-field 'X R3-rect))
    (define Y (literal-vector-field 'Y R3-rect))
    (define Z (literal-vector-field 'Z R3-rect))
    (define W (literal-vector-field 'W R3-rect))
    (define alpha
      (compose (literal-function 'alpha (-> (UP Real Real Real) Real))
               (R3-rect '->coords)))
    (define beta
      (compose (literal-function 'beta (-> (UP Real Real Real) Real))
               (R3-rect '->coords)))
    (define gamma
      (compose (literal-function 'gamma (-> (UP Real Real Real) Real))
               (R3-rect '->coords)))
    (define omega
      (+ (* alpha (wedge dx dy))
         (* beta (wedge dy dz))
         (* gamma (wedge dz dx))))
    (define ((L1 X) omega)
      (+ ((interior-product X) (d omega))
         (d ((interior-product X) omega))))
    (check-simplified? ((- (((Lie-derivative X) omega) Y Z)
                           (((L1 X) omega) Y Z))
                        ((R3-rect '->point) (up 'x0 'y0 'z0)))
                       0)
    (check-simplified? (let ((omega (literal-1form-field 'omega R3-rect)))
                         ((- (((Lie-derivative X) omega) Y)
                             (((L1 X) omega) Y))
                          ((R3-rect '->point) (up 'x0 'y0 'z0))))
                       0)
    (check-simplified? (let ((omega (* alpha (wedge dx dy dz))))
                         ((- (((Lie-derivative X) omega) Y Z W)
                             (((L1 X) omega) Y Z W))
                          ((R3-rect '->point) (up 'x0 'y0 'z0))))
                       0))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))