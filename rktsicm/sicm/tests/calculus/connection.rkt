#lang racket/base

(require rackunit
         "../../main.rkt"
         "../../rkt/int.rkt"
         "../helper.rkt"
         )

(define kernel-tests
  (test-suite
   "calculus/connection"
   (test-case "ORIG:metric->Christoffel-1"
              (define 2-sphere R2-rect)
              (define-coordinates (up theta phi) 2-sphere)

              (define ((g-sphere R) u v)
                (* (square R)
                   (+ (* (dtheta u) (dtheta v))
                      (* (compose (square sin) theta)
                         (dphi u)
                         (dphi v)))))

              (check-simplified?
               ((Christoffel->symbols
                  (metric->Christoffel-1 (g-sphere 'R)
                                         (coordinate-system->basis 2-sphere)))
                 ((2-sphere '->point) (up 'theta0 'phi0)))
               '(down
                 (down (down 0 0) (down 0 (* (expt R 2) (sin theta0) (cos theta0))))
                 (down (down 0 (*  (expt R 2) (sin theta0) (cos theta0)))
                       (down (* -1 (expt R 2) (sin theta0) (cos theta0)) 0)))))

   (test-case "ORIG:metric->Christoffel-2"
              (define 2-sphere R2-rect)
              (define-coordinates (up theta phi) 2-sphere)

              (define ((g-sphere R) u v)
                (* (square R)
                   (+ (* (dtheta u) (dtheta v))
                      (* (compose (square sin) theta)
                         (dphi u)
                         (dphi v)))))

              (check-simplified?
               ((Christoffel->symbols
                  (metric->Christoffel-2 (g-sphere 'R)
                                         (coordinate-system->basis 2-sphere)))
                 ((2-sphere '->point) (up 'theta0 'phi0)))
               '(down (down (up 0 0)
                            (up 0 (/ (cos theta0) (sin theta0))))
                      (down (up 0 (/ (cos theta0) (sin theta0)))
                            (up (* -1 (sin theta0) (cos theta0)) 0)))))

   (test-case "ORIG:Christofel-1"
              (define-coordinates (up x y) R2-rect)

              (define fa
                (compose (literal-function 'a (-> (UP Real Real) Real))
                         (R2-rect '->coords)))
              (define fb
                (compose (literal-function 'b (-> (UP Real Real) Real))
                         (R2-rect '->coords)))
              (define fc
                (compose (literal-function 'c (-> (UP Real Real) Real))
                         (R2-rect '->coords)))

              (define ((g-R2 g_00 g_01 g_11) u v)
                (+ (* g_00 (dx u) (dx v))
                   (* g_01 (+ (* (dx u) (dy v)) (* (dy u) (dx v))))
                   (* g_11 (dy u) (dy v))))

              (check-simplified?
               (((g-R2 fa fb fc)
                           (literal-vector-field 'u R2-rect)
                           (literal-vector-field 'v R2-rect))
                          ((R2-rect '->point) (up 'x0 'y0)))
               '(+ (* (v^0 (up x0 y0)) (u^0 (up x0 y0)) (a (up x0 y0)))
                   (* (v^0 (up x0 y0)) (b (up x0 y0)) (u^1 (up x0 y0)))
                   (* (u^0 (up x0 y0)) (v^1 (up x0 y0)) (b (up x0 y0)))
                   (* (v^1 (up x0 y0)) (u^1 (up x0 y0)) (c (up x0 y0)))))

              (define R2-basis (coordinate-system->basis R2-rect))

              (check-simplified?
               ((Christoffel->symbols
                           (metric->Christoffel-1 (g-R2 fa fb fc) R2-basis))
                          ((R2-rect '->point) (up 'x0 'y0)))
               '(down
                 (down
                  (down (* 1/2 (((partial 0) a) (up x0 y0)))
                        (+ (* -1/2 (((partial 1) a) (up x0 y0)))
                           (((partial 0) b) (up x0 y0))))
                  (down (* 1/2 (((partial 1) a) (up x0 y0)))
                        (* 1/2 (((partial 0) c) (up x0 y0)))))
                 (down
                  (down (* 1/2 (((partial 1) a) (up x0 y0)))
                        (* 1/2 (((partial 0) c) (up x0 y0))))
                  (down (+ (((partial 1) b) (up x0 y0))
                           (* -1/2 (((partial 0) c) (up x0 y0))))
                        (* 1/2 (((partial 1) c) (up x0 y0)))))))

              )

   (test-case "ORIG:Christofel-2"
              (define 2-sphere R2-rect)
              (define-coordinates (up theta phi) 2-sphere)

              (define ((g-sphere R) u v)
                (* (square R)
                   (+ (* (dtheta u) (dtheta v))
                      (* (compose (square sin) theta)
                         (dphi u)
                         (dphi v)))))
              
              (check-simplified?
               ((Christoffel->symbols
                  (metric->Christoffel-2 (g-sphere 'R)
                                         (coordinate-system->basis 2-sphere)))
                 ((2-sphere '->point) (up 'theta0 'phi0)))
               '(down
                 (down (up 0 0) (up 0 (/ (cos theta0) (sin theta0))))
                 (down (up 0 (/ (cos theta0) (sin theta0)))
                       (up (* -1 (sin theta0) (cos theta0)) 0)))))

   (test-case "ORIG:Cartan"
              (define Cartan (literal-Cartan 'G R2-rect))
              (define CF (Cartan->forms Cartan))
              (define polar R2-polar)
              (define-coordinates (up r theta) polar)
              (define polar-basis (coordinate-system->basis polar))
              (define (polar-metric v1 v2)
                (+ (* (dr v1) (dr v2))
                   (* (square r)
                      (* (dtheta v1) (dtheta v2)))))

              (check-simplified?
               ((Christoffel->symbols
                  (metric->Christoffel-2 polar-metric polar-basis))
                 ((polar '->point) (up 'r 'theta)))
               '(down
                 (down (up 0 0)
                       (up 0 (/ 1 r)))
                 (down (up 0 (/ 1 r))
                       (up (* -1 r) 0)))))

   (test-case "ORIG:spherical-metric"
              (define spherical R3-rect)
              (define-coordinates (up r theta phi) spherical)
              (define spherical-basis (coordinate-system->basis spherical))
              (define (spherical-metric v1 v2)
                (+ (* (dr v1) (dr v2))
                   (* (square r)
                      (+ (* (dtheta v1) (dtheta v2))
                         (* (expt (sin theta) 2)
                            (dphi v1) (dphi v2))))))

              (check-simplified?
               ((Christoffel->symbols
                  (metric->Christoffel-2 spherical-metric spherical-basis))
                 ((spherical '->point) (up 'r 'theta 'phi)))
               '(down
                 (down (up 0 0 0) (up 0 (/ 1 r) 0) (up 0 0 (/ 1 r)))
                 (down (up 0 (/ 1 r) 0) (up (* -1 r) 0 0) (up 0 0 (/ (cos theta) (sin theta))))
                 (down (up 0 0 (/ 1 r))
                       (up 0 0 (/ (cos theta) (sin theta)))
                       (up (+ (* r (expt (cos theta) 2)) (* -1 r))
                           (* -1 (cos theta) (sin theta))
                           0)))))

   (test-case "ORIG:flat-lorentz"
              ;;; MTW p205 spherical flat lorentz

              (define spherical-Lorentz R4-rect)
              (define-coordinates (up t r theta phi) spherical-Lorentz)

              (define spherical-Lorentz-basis
                (coordinate-system->basis spherical-Lorentz))

              (define ((spherical-Lorentz-metric c^2) v1 v2)
                (+ (* -1 c^2 (* (dt v1) (dt v2)))
                   (* (dr v1) (dr v2))
                   (* (square r)
                      (+ (* (dtheta v1) (dtheta v2))
                         (* (square (sin theta))
                            (* (dphi v1) (dphi v2)))))))

              (define spherical-Lorentz-point 
                ((spherical-Lorentz '->point) (up 't 'r 'theta 'phi)))

              (define (orthonormal-spherical-Lorentz-vector-basis c^2)
                (down (* (/ 1 (sqrt c^2)) d/dt)
                      d/dr
                      (* (/ 1 r) d/dtheta)
                      (* (/ 1 (* r (sin theta))) d/dphi)))

              (define (orthonormal-spherical-Lorentz-1form-basis c^2)
                (let ((orthonormal-spherical-Lorentz-vectors
                       (orthonormal-spherical-Lorentz-vector-basis c^2)))
                  (vector-basis->dual orthonormal-spherical-Lorentz-vectors 
                                      spherical-Lorentz)))

              (define (orthonormal-spherical-Lorentz-basis c^2)
                (make-basis (orthonormal-spherical-Lorentz-vector-basis c^2)
                            (orthonormal-spherical-Lorentz-1form-basis c^2)))

              (check-simplified?
               ((s:map/r (orthonormal-spherical-Lorentz-1form-basis 'c^2)
                                   (orthonormal-spherical-Lorentz-vector-basis 'c^2))
                          spherical-Lorentz-point)
               '(down (up 1 0 0 0) (up 0 1 0 0) (up 0 0 1 0) (up 0 0 0 1)))


              (check-simplified?
               (((spherical-Lorentz-metric 'c^2)
                  (ref (orthonormal-spherical-Lorentz-vector-basis 'c^2) 0)
                  (ref (orthonormal-spherical-Lorentz-vector-basis 'c^2) 0))
                 spherical-Lorentz-point)
               -1)

              (check-simplified?
               (((spherical-Lorentz-metric 'c^2)
                  (ref (orthonormal-spherical-Lorentz-vector-basis 'c^2) 1)
                  (ref (orthonormal-spherical-Lorentz-vector-basis 'c^2) 1))
                 spherical-Lorentz-point)
               1)

              (check-simplified?
               (((spherical-Lorentz-metric 'c^2)
                     (ref (orthonormal-spherical-Lorentz-vector-basis 'c^2) 2)
                     (ref (orthonormal-spherical-Lorentz-vector-basis 'c^2) 2))
                    spherical-Lorentz-point)
               1)

              (check-simplified?
               (((spherical-Lorentz-metric 'c^2)
                     (ref (orthonormal-spherical-Lorentz-vector-basis 'c^2) 3)
                     (ref (orthonormal-spherical-Lorentz-vector-basis 'c^2) 3))
                    spherical-Lorentz-point)
               1)

              (check-simplified?
               ((Christoffel->symbols
                  (metric->connection-1 (spherical-Lorentz-metric 'c^2)
                                        (orthonormal-spherical-Lorentz-basis 'c^2)))
                 spherical-Lorentz-point)
               '(down
                 (down (down 0 0 0 0) (down 0 0 0 0) (down 0 0 0 0) (down 0 0 0 0))
                 (down (down 0 0 0 0) (down 0 0 0 0) (down 0 0 0 0) (down 0 0 0 0))
                 (down (down 0 0 0 0) (down 0 0 (/ 1 r) 0) (down 0 (/ -1 r) 0 0) (down 0 0 0 0))
                 (down (down 0 0 0 0)
                       (down 0 0 0 (/ 1 r))
                       (down 0 0 0 (/ (cos theta) (* r (sin theta))))
                       (down 0 (/ -1 r) (/ (* -1 (cos theta)) (* r (sin theta))) 0))))

              (define foo
                (time
                 ((Christoffel->symbols
                   (metric->connection-2 (spherical-Lorentz-metric 'c^2)
                                         (orthonormal-spherical-Lorentz-basis 'c^2)))
                  spherical-Lorentz-point)))
              (check-simplified?
               foo
               '(down
                 (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
                 (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
                 (down (up 0 0 0 0) (up 0 0 (/ 1 r) 0) (up 0 (/ -1 r) 0 0) (up 0 0 0 0))
                 (down (up 0 0 0 0)
                       (up 0 0 0 (/ 1 r))
                       (up 0 0 0 (/ (cos theta) (* r (sin theta))))
                       (up 0 (/ -1 r) (/ (* -1 (cos theta)) (* r (sin theta))) 0))))
              ;;; The last two are essentially the same.  Is this correct?

              ;;; Check answers from MTW p.213
              ;;; t r theta phi
              ;;; 0 1 2     3

              (check-simplified?
               (ref foo 3 2 3)
               '(/ (cos theta) (* r (sin theta))))

              (check-simplified?
               (ref foo 3 3 2)
               '(/ (* -1 (cos theta)) (* r (sin theta))))

              (check-simplified?
               (ref foo 2 1 2)
               '(/ 1 r))

              (check-simplified?
               (ref foo 3 1 3)
               '(/ 1 r))

              (check-simplified?
               (ref foo 2 2 1)
               '(/ -1 r))

              (check-simplified?
               (ref foo 3 3 1)
               '(/ -1 r))

              )

   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests kernel-tests))