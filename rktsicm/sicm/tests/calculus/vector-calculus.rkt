#lang racket/base

(require rackunit
         "../../main.rkt"
         "../helper.rkt"
         )

(provide the-tests)
(define the-tests
  (test-suite
   "calculus/vector-calculus"
   (test-case
    "Test setup for spherical system"
    (define spherical R3-rect)
    (define-coordinates (up r theta phi) spherical)
    (define spherical-point
      ((point spherical) (up 'r 'theta 'phi)))
    (define spherical-basis
      (coordinate-system->basis spherical))
    (define (spherical-metric v1 v2)
      (+ (* (dr v1) (dr v2))
         (* (square r)
            (+ (* (dtheta v1) (dtheta v2))
               (* (expt (sin theta) 2)
                  (dphi v1) (dphi v2))))))
    (define spherical-Gamma
      (make-Christoffel
       (let ((O (lambda x 0)))
         (down
          (down (up O O O)
                (up O (/ 1 r) O)
                (up O O (/ 1 r)))
          (down (up O (/ 1 r) O)
                (up (* -1 r) O O)
                (up O O (/ (cos theta) (sin theta))))
          (down (up O O (/ 1 r))
                (up O O (/ (cos theta) (sin theta)))
                (up (* -1 r (expt (sin theta) 2))
                    (* -1 (sin theta) (cos theta))
                    O))))
       (coordinate-system->basis spherical)))
    (define spherical-Cartan
      (Christoffel->Cartan spherical-Gamma))
    ;;; normalized spherical basis
    (define e_0 d/dr)
    (define e_1 (* (/ 1 r) d/dtheta))
    (define e_2 (* (/ 1 (* r (sin theta))) d/dphi))
    (check-simplified? ((spherical-metric e_0 e_0) spherical-point) 1)
    (check-simplified? ((spherical-metric e_1 e_1) spherical-point) 1)
    (check-simplified? ((spherical-metric e_2 e_2) spherical-point) 1)
    (check-simplified? ((spherical-metric e_0 e_1) spherical-point) 0)
    (check-simplified? ((spherical-metric e_0 e_2) spherical-point) 0)
    (check-simplified? ((spherical-metric e_1 e_2) spherical-point) 0)
    (define orthonormal-spherical-vector-basis
      (down e_0 e_1 e_2))
    (define orthonormal-spherical-1form-basis
      (vector-basis->dual orthonormal-spherical-vector-basis
                          spherical))
    (define orthonormal-spherical-basis
      (make-basis orthonormal-spherical-vector-basis
                  orthonormal-spherical-1form-basis))
    (define v
      (+ (* (literal-manifold-function 'v^0 spherical) e_0)
         (* (literal-manifold-function 'v^1 spherical) e_1)
         (* (literal-manifold-function 'v^2 spherical) e_2)))
    ;;; Test of Gradient
    (check-simplified? ((orthonormal-spherical-1form-basis
                         ((gradient spherical-metric spherical-basis)
                          (literal-manifold-function 'f spherical)))
                        spherical-point)
                       '(up (((partial 0) f) (up r theta phi))
                            (/ (((partial 1) f) (up r theta phi)) r)
                            (/ (((partial 2) f) (up r theta phi)) (* r (sin theta)))))
    ;;; Test of Curl
    (check-simplified? ((orthonormal-spherical-1form-basis
                         ((curl spherical-metric orthonormal-spherical-basis) v))
                        spherical-point)
                       '(up
                         (/ (+ (* (sin theta) (((partial 1) v^2) (up r theta phi)))
                               (* (cos theta) (v^2 (up r theta phi)))
                               (* -1 (((partial 2) v^1) (up r theta phi))))
                            (* r (sin theta)))
                         (/ (+ (* -1 r (sin theta) (((partial 0) v^2) (up r theta phi)))
                               (* -1 (sin theta) (v^2 (up r theta phi)))
                               (((partial 2) v^0) (up r theta phi)))
                            (* r (sin theta)))
                         (/ (+ (* r (((partial 0) v^1) (up r theta phi)))
                               (v^1 (up r theta phi))
                               (* -1 (((partial 1) v^0) (up r theta phi))))
                            r)))
    ;;; Test of Divergence
    (check-simplified? (((divergence spherical-metric orthonormal-spherical-basis) v)
                        spherical-point)
                       '(+ (((partial 0) v^0) (up r theta phi))
                           (/ (* 2 (v^0 (up r theta phi))) r)
                           (/ (((partial 1) v^1) (up r theta phi)) r)
                           (/ (* (v^1 (up r theta phi)) (cos theta)) (* r (sin theta)))
                           (/ (((partial 2) v^2) (up r theta phi)) (* r (sin theta)))))
    (let ([phi (literal-manifold-function 'phi spherical)])
      (check-simplified? (((Laplacian spherical-metric orthonormal-spherical-basis)
                           phi)
                          spherical-point)
                         '(+ (((partial 0) ((partial 0) phi)) (up r theta phi))
                             (/ (* 2 (((partial 0) phi) (up r theta phi)))
                                r)
                             (/ (((partial 1) ((partial 1) phi)) (up r theta phi))
                                (expt r 2))
                             (/ (* (cos theta) (((partial 1) phi) (up r theta phi)))
                                (* (expt r 2) (sin theta)))
                             (/ (((partial 2) ((partial 2) phi)) (up r theta phi))
                                (* (expt r 2) (expt (sin theta) 2)))))))
   (test-case
    "R4-rect"
    ;;; Obtaining the wave equation.
    (define SR R4-rect)
    (define-coordinates (up t x y z) SR)
    (define an-event ((point SR) (up 't0 'x0 'y0 'z0)))
    (define c 'c)         ; We like units.
    (define (g-Minkowski u v)
      (+ (* -1 (square c) (dt u) (dt v))
         (* (dx u) (dx v))
         (* (dy u) (dy v))
         (* (dz u) (dz v))))
    (define SR-vector-basis
      (down (* (/ 1 c) d/dt) d/dx d/dy d/dz))
    (define SR-1form-basis
      (up (* c dt) dx dy dz))
    (define SR-basis
      (make-basis SR-vector-basis
                  SR-1form-basis))
    (check-simplified? (s:map/r
                        (lambda (u)
                          (s:map/r (lambda (v)
                                     ((g-Minkowski u v) an-event))
                                   SR-vector-basis))
                        SR-vector-basis)
                       '(down (down -1 0 0 0)
                              (down  0 1 0 0)
                              (down  0 0 1 0)
                              (down  0 0 0 1)))
    
    (let ([phi (literal-manifold-function 'phi SR)])
      (check-simplified? (((Laplacian g-Minkowski SR-basis) phi) an-event)
                         '(+ (* -1 (((partial 1) ((partial 1) phi)) (up t0 x0 y0 z0)))
                             (* -1 (((partial 2) ((partial 2) phi)) (up t0 x0 y0 z0)))
                             (* -1 (((partial 3) ((partial 3) phi)) (up t0 x0 y0 z0)))
                             (/ (((partial 0) ((partial 0) phi)) (up t0 x0 y0 z0)) (expt c 2))))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))