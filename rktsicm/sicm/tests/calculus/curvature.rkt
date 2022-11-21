#lang racket/base

(require rackunit
         "../../main.rkt"
         "../helper.rkt"
         )

(void
 (clear-arguments)
 (suppress-arguments '((up x0 y0)))
 (rename-part 'derivative 'D))

(define the-tests
  (test-suite
   "calculus/curvature"
   (test-case
    "General torsion"
    ;;; General torsion is not too complicated to compute 
    (define-coordinates (up x y) R2-rect)
    (define R2-rect-basis
      (coordinate-system->basis R2-rect))
    (define R2-rect-point
      ((R2-rect '->point) (up 'x0 'y0)))
    (define (Gijk i j k)
      (literal-manifold-function
       (string->symbol
        (string-append "G^"
                       (number->string i)
                       "_"
                       (number->string j)
                       (number->string k)))
       R2-rect))
				    
    (define G
      (down (down (up (Gijk 0 0 0)
                      (Gijk 1 0 0))
                  (up (Gijk 0 1 0)
                      (Gijk 1 1 0)))
            (down (up (Gijk 0 0 1)
                      (Gijk 1 0 1))
                  (up (Gijk 0 1 1)
                      (Gijk 1 1 1)))))
    (define CG (make-Christoffel G R2-rect-basis))

    (define CF (Christoffel->Cartan CG))

    (define v (literal-vector-field 'v R2-rect))
    (define w (literal-vector-field 'w R2-rect))
    (define f (literal-manifold-function 'f R2-rect))
    (check-simplified? ((((torsion-vector (covariant-derivative CF)) v w) f)
                        R2-rect-point)
                       '(+ (* -1 G^1_01 v^0 ((partial 1) f) w^1)
                           (* G^1_01 w^0 ((partial 1) f) v^1)
                           (* -1 G^0_01 v^0 ((partial 0) f) w^1)
                           (* G^0_01 w^0 ((partial 0) f) v^1)
                           (* G^1_10 v^0 ((partial 1) f) w^1)
                           (* -1 G^1_10 w^0 ((partial 1) f) v^1)
                           (* G^0_10 v^0 ((partial 0) f) w^1)
                           (* -1 G^0_10 w^0 ((partial 0) f) v^1)))
    ;;; Unfortunately, this says only that the 
    ;;; Christoffel symbols are symmetric in the
    ;;; lower two indices iff the torsion is zero.
    )
   (test-case
    "Spherical"
    (define-coordinates (up theta phi) S2-spherical)
    (define S2-spherical-basis (coordinate-system->basis S2-spherical))
    (define a-point ((S2-spherical '->point) (up 'theta 'phi)))
    (define a-function (literal-scalar-field 'f S2-spherical))
    ;;; the Christoffel symbols (for r=1) (p.341 mtw) are:
    ;;; (the up-down-down Christoffel symbols do not depend on R)
    (define G-S2-1
      (make-Christoffel
       (let ((zero  (lambda (point) 0))) 
         (down (down (up zero zero)
                     (up zero (/ 1 (tan theta))))
               (down (up zero (/ 1 (tan theta)))
                     (up (- (* (sin theta) (cos theta))) zero))))
       S2-spherical-basis))

    (check-simplified? (((commutator d/dtheta d/dphi) a-function) a-point)
                      0)
    (check-simplified? (let ((nabla
                              (covariant-derivative
                               (Christoffel->Cartan G-S2-1))))
                         ((((nabla d/dtheta) d/dphi)
                           a-function)
                          a-point))
                       '(/ (* (cos theta)
                              (((partial 1) f) (up theta phi)))
                           (sin theta)))
    (check-simplified? (let ((nabla
                              (covariant-derivative
                               (Christoffel->Cartan G-S2-1))))
                         ((((nabla d/dphi) ((nabla d/dtheta) d/dphi))
                           a-function)
                          a-point))
                       '(* -1 (((partial 0) f) (up theta phi)) (expt (cos theta) 2)))
    (check-simplified? (let ((nabla
                              (covariant-derivative
                               (Christoffel->Cartan G-S2-1))))
                         (define ans '())
                         (for-each
                          (lambda (x)
                            (for-each
                             (lambda (y)
                               (set! ans (append ans (list (simplify ((((torsion-vector nabla) x y)
                                                                       a-function)
                                                                      a-point))))))
                             (list  d/dtheta d/dphi)))
                          (list  d/dtheta d/dphi))
                         ans)
                       '(0 0 0 0))
    (check-simplified? (let ((nabla
                              (covariant-derivative
                               (Christoffel->Cartan G-S2-1))))
                         (((Riemann nabla)
                                dphi d/dtheta d/dphi d/dtheta)
                               a-point))
                       1))
   (test-case
    "sphere in R2"
    ;;; We can work without embedding the sphere in R^3
    ;;; We need another copy of R2...
    (define S2-spherical-basis (coordinate-system->basis S2-spherical))
    (define M (make-manifold R^n 2))
    (define M-rect (coordinate-system-at 'rectangular 'origin M))
    (define M-polar (coordinate-system-at 'polar/cylindrical 'origin M))
    (define-coordinates (up theta phi) M-rect)
    (define M-basis (coordinate-system->basis M-rect))
    (define a-point ((M-rect '->point) (up 'theta0 'phi0)))
    (define a-function (literal-scalar-field 'f M-rect))
    (define G-S2-1
      (make-Christoffel
       (let ((zero  (lambda (point) 0))) 
         (down (down (up zero zero)
                     (up zero (/ 1 (tan theta))))
               (down (up zero (/ 1 (tan theta)))
                     (up (- (* (sin theta) (cos theta))) zero))))
       M-basis))
    (check-simplified? (let ((nabla
                              (covariant-derivative
                               (Christoffel->Cartan G-S2-1))))
                         (define ans '())
                         (for-each
                          (lambda (x)
                            (for-each
                             (lambda (y)
                               (set! ans (append ans (list (simplify ((((torsion-vector nabla) x y)
                                                                       a-function)
                                                                      a-point))))))
                             (list  d/dtheta d/dphi)))
                          (list  d/dtheta d/dphi))
                         ans)
                       '(0 0 0 0))
    (check-simplified? (let ((nabla
                              (covariant-derivative
                               (Christoffel->Cartan G-S2-1))))
                         (((Riemann nabla)
                           dphi d/dtheta d/dphi d/dtheta)
                          a-point))
                       1)
    ;(set! *divide-out-terms* #f) ;;bdk;; not (yet) implemented
    ;;; R^alpha_{beta gamma delta}
    (check-simplified? (let ((nabla
                              (covariant-derivative
                               (Christoffel->Cartan G-S2-1))))
                         (define ans '())
                         (for-each
                          (lambda (alpha)
                            (for-each
                             (lambda (beta)
                               (for-each
                                (lambda (gamma)
                                  (for-each
                                   (lambda (delta)
                                     (set! ans (append ans `(((,alpha ,beta ,gamma ,delta)
                                                              ,(simplify (((Riemann nabla)
                                                                           alpha beta gamma delta)
                                                                          a-point)))))))
                                   (list d/dtheta d/dphi)))
                                (list d/dtheta d/dphi)))
                             (list d/dtheta d/dphi)))
                          (list dtheta dphi))
                         ans)
                       '(((dtheta d/dtheta d/dtheta d/dtheta) 0)
                         ((dtheta d/dtheta d/dtheta d/dphi) 0)
                         ((dtheta d/dtheta d/dphi d/dtheta) 0)
                         ((dtheta d/dtheta d/dphi d/dphi) 0)
                         ((dtheta d/dphi d/dtheta d/dtheta) 0)
                         ((dtheta d/dphi d/dtheta d/dphi) (expt (sin theta0) 2))
                         ((dtheta d/dphi d/dphi d/dtheta) (* -1 (expt (sin theta0) 2)))
                         ((dtheta d/dphi d/dphi d/dphi) 0)
                         ((dphi d/dtheta d/dtheta d/dtheta) 0)
                         ((dphi d/dtheta d/dtheta d/dphi) -1)
                         ((dphi d/dtheta d/dphi d/dtheta) 1)
                         ((dphi d/dtheta d/dphi d/dphi) 0)
                         ((dphi d/dphi d/dtheta d/dtheta) 0)
                         ((dphi d/dphi d/dtheta d/dphi) 0)
                         ((dphi d/dphi d/dphi d/dtheta) 0)
                         ((dphi d/dphi d/dphi d/dphi) 0)))
    ;;; Equation of geodesic deviation (MTW p275 eq.11.10) has a type
    ;;; error.  The variation is ambiguously a vector-field over a map and
    ;;; a vector field.  Riemann must take uniform stuff, and U is a
    ;;; vector field on N (the-real-line), however variation is defined
    ;;; only over the map.  The following does not work!
    #;( ;;bdk;; --- covariant-derivative-over-map => not defined...
       check-exn #px"Wrong type argument -- LITERAL-FUNCTION"
               (λ () (let ((U (components->vector-field (lambda (x) 1) the-real-line 'U))
                           (mu:N->M (compose (M '->point)
                                             (up (literal-function 'f^theta)
                                                 (literal-function 'f^phi)))))
                       (let* ((basis-over-mu (basis->basis-over-map mu:N->M S2-spherical-basis))
                              (1form-basis (basis->1form-basis basis-over-mu))
                              (vector-basis (basis->vector-basis basis-over-mu))
                              (Cartan (Christoffel->Cartan G-S2-1))
                              (variation (basis-components->vector-field
                                          (up (literal-function 'd_theta)
                                              (literal-function 'd_phi))
                                          vector-basis))
                              (nabla (covariant-derivative-over-map Cartan mu:N->M))
                              (nablau (nabla U))
                              (d1 (nablau (nablau variation)))
                              (d2 (((Riemann-curvature nabla) variation U) U))
                              (deviation (+ d1 d2)))
                         (s:map/r 
                          (lambda (w)
                            ((w deviation) ((the-real-line '->point) 'tau)))
                          1form-basis)))))
    ;;; OK, in considering the variational problem, the map is actually
    ;;; two dimensional, time is one direction and variation the other.
    ;;; The Christoffel symbols (for R=1) (p.341 MTW) are:
    ;;; (the up-down-down Christoffel symbols do not depend on R)
    (define-coordinates (up t n) R2-rect)
    (define f^theta (literal-function 'f^theta (-> (UP Real Real) Real)))
    (define f^phi (literal-function 'f^phi (-> (UP Real Real) Real)))
    (define (SSS a b c) (substitute (simplify a) (simplify b) (simplify c)))
    (define s0
      (simplify
       (let* ( ;; d/dt and d/dn exist
              (mu:N->M (compose (M-rect '->point)
                                (up f^theta f^phi)
                                (R2-rect '->coords)))
              (basis-over-mu (basis->basis-over-map mu:N->M M-basis))
              (1form-basis (basis->1form-basis basis-over-mu))
              (Cartan (Christoffel->Cartan G-S2-1))
              (nabla (covariant-derivative Cartan mu:N->M))
              (nablau (nabla d/dt))
              (d1 (nablau (nablau ((differential mu:N->M) d/dn))))
              (d2 (((Riemann-curvature nabla) d/dn d/dt)
                   ((differential mu:N->M) d/dt)))
              (deviation (+ d1 d2)))
         (s:map/r 
          (lambda (w)
            ((w deviation) ((R2-rect '->point) (up 'tau 0))))
          1form-basis))))
    (define s1  (SSS 'xidotdot  '(((partial 0) ((partial 0) ((partial 1) f^theta))) (up tau 0)) s0))
    (define s2  (SSS 'etadotdot '(((partial 0) ((partial 0) ((partial 1) f^phi))) (up tau 0)) s1))
    (define s3  (SSS 'phidotdot '(((partial 0) ((partial 0) f^phi)) (up tau 0)) s2))
    (define s4  (SSS 'thetadotdot '(((partial 0) ((partial 0) f^theta)) (up tau 0)) s3))
    (define s5  (SSS 'etadot    '(((partial 0) ((partial 1) f^phi)) (up tau 0)) s4))
    (define s6  (SSS 'xidot     '(((partial 0) ((partial 1) f^theta)) (up tau 0)) s5))
    (define s7  (SSS 'xi        '(((partial 1) f^theta) (up tau 0)) s6))
    (define s8  (SSS 'eta       '(((partial 1) f^phi) (up tau 0)) s7))
    (define s9  (SSS 'thetadot  '(((partial 0) f^theta) (up tau 0)) s8))
    (define s10 (SSS 'phidot    '(((partial 0) f^phi) (up tau 0)) s9))
    (define s11 (SSS 'theta     '(f^theta (up tau 0)) s10))
    (define s12 (SSS 'phi       '(f^phi (up tau 0)) s11))
    ;;; Substituting from the geodesic equation (equation of motion) to
    ;;; make make use of the fact that the trajectory is a geodesic.
    (define s13 (SSS '(* -2 thetadot phidot (/ (cos theta) (sin theta))) 'phidotdot s12))
    (define s14 (SSS '(* phidot phidot (cos theta) (sin theta)) 'thetadotdot s13))
    (check-simplified? s14 '(up
                             (+ (* -2 (expt phidot 2) xi (expt (cos theta) 2))
                                (* -2 etadot phidot (cos theta) (sin theta))
                                (* (expt phidot 2) xi)
                                xidotdot)
                             (/
                              (+ (* 2 etadot thetadot (cos theta) (sin theta))
                                 (* 2 phidot xidot (cos theta) (sin theta))
                                 (* etadotdot (expt (sin theta) 2))
                                 (* -2 phidot thetadot xi))
                              (expt (sin theta) 2))))
    ;Testing equation 3 on MTW p272
    (define n0
      (simplify
       (let* ( ;; d/dt and d/dn exist
              (mu:N->M (compose 
                        (M-rect '->point)
                        (up f^theta f^phi)
                        (R2-rect '->coords)))
              (basis-over-mu (basis->basis-over-map mu:N->M M-basis))
              (1form-basis (basis->1form-basis basis-over-mu))
              (Cartan (Christoffel->Cartan G-S2-1))
              (nabla (covariant-derivative Cartan mu:N->M))
              (nablau (nabla d/dt))
              (nablan (nabla d/dn))
              (deviation (nablan (nablau ((differential mu:N->M) d/dt)))))
         (s:map/r 
          (lambda (w)
            ((w deviation) ((R2-rect '->point) (up 'tau 0))))
          1form-basis))))
    ;do all substitutions again...
    (define n1  (SSS 'xidotdot  '(((partial 0) ((partial 0) ((partial 1) f^theta))) (up tau 0)) n0))
    (define n2  (SSS 'etadotdot '(((partial 0) ((partial 0) ((partial 1) f^phi))) (up tau 0)) n1))
    (define n3  (SSS 'phidotdot '(((partial 0) ((partial 0) f^phi)) (up tau 0)) n2))
    (define n4  (SSS 'thetadotdot '(((partial 0) ((partial 0) f^theta)) (up tau 0)) n3))
    (define n5  (SSS 'etadot    '(((partial 0) ((partial 1) f^phi)) (up tau 0)) n4))
    (define n6  (SSS 'xidot     '(((partial 0) ((partial 1) f^theta)) (up tau 0)) n5))
    (define n7  (SSS 'xi        '(((partial 1) f^theta) (up tau 0)) n6))
    (define n8  (SSS 'eta       '(((partial 1) f^phi) (up tau 0)) n7))
    (define n9  (SSS 'thetadot  '(((partial 0) f^theta) (up tau 0)) n8))
    (define n10 (SSS 'phidot    '(((partial 0) f^phi) (up tau 0)) n9))
    (define n11 (SSS 'theta     '(f^theta (up tau 0)) n10))
    (define n12 (SSS 'phi       '(f^phi (up tau 0)) n11))
    (check-simplified? n12 '(up
                             (+ (* -2 eta phidot thetadot (expt (cos theta) 2))
                                (* -2 (expt phidot 2) xi (expt (cos theta) 2))
                                (* -1 eta phidotdot (cos theta) (sin theta))
                                (* -2 etadot phidot (cos theta) (sin theta))
                                (* (expt phidot 2) xi)
                                xidotdot)
                             (/
                              (+ (* -1 eta (expt phidot 2) (expt (cos theta) 2) (sin theta))
                                 (* -2 phidot thetadot xi (sin theta))
                                 (* eta thetadotdot (cos theta))
                                 (* 2 etadot thetadot (cos theta))
                                 (* 2 phidot xidot (cos theta))
                                 (* phidotdot xi (cos theta))
                                 (* etadotdot (sin theta)))
                              (sin theta))))
    (define n13 (SSS '(* -2 thetadot phidot (/ (cos theta) (sin theta))) 'phidotdot n12))
    (define n14 (SSS '(* phidot phidot (cos theta) (sin theta)) 'thetadotdot n13))
    (check-simplified? n14 '(up
                             (+ (* -2 (expt phidot 2) xi (expt (cos theta) 2))
                                (* -2 etadot phidot (cos theta) (sin theta))
                                (* (expt phidot 2) xi)
                                xidotdot)
                             (/
                              (+ (* 2 etadot thetadot (cos theta) (sin theta))
                                 (* 2 phidot xidot (cos theta) (sin theta))
                                 (* etadotdot (expt (sin theta) 2))
                                 (* -2 phidot thetadot xi))
                              (expt (sin theta) 2))))
    ;agrees with Riemann calculation
    ;shouldn't this be zero?
    )
   (test-case
    "parallel transport of vector about a loop"
    (define-coordinates t the-real-line)
    ;;; The coordinates on the unit sphere
    (define-coordinates (up theta phi) S2-spherical)
    (define S2-spherical-basis (coordinate-system->basis S2-spherical))
    ;;; The Christoffel symbols (for r=1) (p.341 MTW) are:
    (define G-S2-1
      (make-Christoffel
       (let ((zero  (lambda (point) 0))) 
         (down (down (up zero zero)
                     (up zero (/ 1 (tan theta))))
               (down (up zero (/ 1 (tan theta)))
                     (up (- (* (sin theta) (cos theta))) zero))))
       S2-spherical-basis))
    ;;; Ordinary Lagrange Equations (= Geodesic Equations)
    (check-simplified? (let ((U d/dt)
                             (mu:N->M (compose (S2-spherical '->point)
                                               (up (literal-function 'f^theta)
                                                   (literal-function 'f^phi))
                                               (the-real-line '->coords))))
                         (let* ((basis-over-mu (basis->basis-over-map mu:N->M S2-spherical-basis))
                                (1form-basis (basis->1form-basis basis-over-mu))
                                (Cartan (Christoffel->Cartan G-S2-1)))
                           (s:map/r 
                            (lambda (w)
                              ((w (((covariant-derivative Cartan mu:N->M) U)
                                   ((differential mu:N->M) U)))
                               ((the-real-line '->point) 'tau)))
                            1form-basis)))
                       '(up
                         (+ (((expt D 2) f^theta) tau)
                            (* -1 (cos (f^theta tau)) (sin (f^theta tau)) (expt ((D f^phi) tau) 2)))
                         (/ (+ (* (sin (f^theta tau)) (((expt D 2) f^phi) tau))
                               (* 2 (cos (f^theta tau)) ((D f^phi) tau) ((D f^theta) tau)))
                            (sin (f^theta tau)))))
    ;;; Parallel transport of vector W over path mu
    (check-simplified? (let ((U d/dt)
                             (mu:N->M (compose (S2-spherical '->point)
                                               (up (literal-function 'f^theta)
                                                   (literal-function 'f^phi))
                                               (the-real-line '->coords))))
                         (let* ((basis-over-mu
                                 (basis->basis-over-map mu:N->M S2-spherical-basis))
                                (1form-basis (basis->1form-basis basis-over-mu))
                                (vector-basis (basis->vector-basis basis-over-mu))
                                (Cartan (Christoffel->Cartan G-S2-1))
                                (transported-vector-over-map 
                                 (basis-components->vector-field
                                  (up (compose (literal-function 'w^0)
                                               (the-real-line '->coords))
                                      (compose (literal-function 'w^1)
                                               (the-real-line '->coords)))
                                  vector-basis)))
                           (s:map/r 
                            (lambda (w)
                              ((w
                                (((covariant-derivative Cartan mu:N->M) U)
                                 transported-vector-over-map))
                               ((the-real-line '->point) 'tau)))
                            1form-basis)))
                       '(up
                         (+ ((D w^0) tau)
                            (* -1 (cos (f^theta tau)) ((D f^phi) tau) (w^1 tau) (sin (f^theta tau))))
                         (/ (+ (* (sin (f^theta tau)) ((D w^1) tau))
                               (* (cos (f^theta tau)) ((D f^phi) tau) (w^0 tau))
                               (* (cos (f^theta tau)) (w^1 tau) ((D f^theta) tau)))
                            (sin (f^theta tau))))
                       ;;; was  ...  looks like right hand side
                       #;(up (* (sin (theta tau)) (cos (theta tau)) (w^1 tau)
                                ((D phi) tau))
                             (/ (+ (* -1 (w^0 tau) (cos (theta tau)) ((D phi) tau))
                                   (* -1 ((D theta) tau) (cos (theta tau)) (w^1 tau)))
                                (sin (theta tau)))))
    ;;; To set up for solving for the derivatives, we lift off of the path
    (check-simplified? (let ((U d/dt)
                             (mu:N->M (compose (S2-spherical '->point)
                                               (up (literal-function 'f^theta)
                                                   (literal-function 'f^phi))
                                               (the-real-line '->coords))))
                         (let* ((basis-over-mu (basis->basis-over-map mu:N->M S2-spherical-basis))
                                (1form-basis (basis->1form-basis basis-over-mu))
                                (vector-basis (basis->vector-basis basis-over-mu))
                                (Cartan (Christoffel->Cartan G-S2-1))
                                (transported-vector-over-map 
                                 (basis-components->vector-field
                                  (up (compose (osculating-path (up 'tau 'w^0 'dw^0/dt))
                                               (the-real-line '->coords))
                                      (compose (osculating-path (up 'tau 'w^1 'dw^1/dt))
                                               (the-real-line '->coords)))
                                  vector-basis)))
                           (s:map/r 
                            (lambda (w)
                              ((w
                                (((covariant-derivative Cartan mu:N->M)
                                  U)
                                 transported-vector-over-map))
                               ((the-real-line '->point) 'tau)))
                            1form-basis)))
                       '(up (+ dw^0/dt
                               (* -1 (cos (f^theta tau)) ((D f^phi) tau) (sin (f^theta tau)) w^1))
                            (/ (+ (* (sin (f^theta tau)) dw^1/dt)
                                  (* (cos (f^theta tau)) ((D f^phi) tau) w^0)
                                  (* (cos (f^theta tau)) ((D f^theta) tau) w^1))
                               (sin (f^theta tau)))))
    ;;; Loaded solve by (load "/usr/local/scmutils/src/solve/linreduce")
    #;(set! *divide-out-terms* #f) ;;bdk;; not defined
    #;(check-simplified? (let ((tau 'tau)
                             (theta (literal-function 'f^theta))
                             (phi (literal-function 'f^phi))
                             (w^0 (literal-function 'w^0))
                             (w^1 (literal-function 'w^1)))
                         (solve
                          (lambda (v)
                            (let ((dw^0/dt (ref v 0))
                                  (dw^1/dt (ref v 1)))
                              (up
                               (+ (* -1
                                     (w^1 tau)
                                     (sin (theta tau))
                                     (cos (theta tau))
                                     ((D phi) tau))
                                  dw^0/dt)
                               (+ (/ (* (w^0 tau) (cos (theta tau)) ((D phi) tau))
                                     (sin (theta tau)))
                                  (/ (* (w^1 tau) ((D theta) tau) (cos (theta tau)))
                                     (sin (theta tau)))
                                  dw^1/dt))))
                          2 2))
                         '(up (* (w^1 tau) (sin (f^theta tau)) (cos (f^theta tau)) ((D f^phi) tau))
                              (/ (+ (* -1 (w^1 tau) (cos (f^theta tau)) ((D f^theta) tau))
                                    (* -1 (cos (f^theta tau)) ((D f^phi) tau) (w^0 tau)))
                                 (sin (f^theta tau)))))
    #;(check-simplified? (let ((U d/dt)
                             (mu:N->M (compose (S2-spherical '->point)
                                               (up (literal-function 'f^theta)
                                                   (literal-function 'f^phi))
                                               (the-real-line '->coords))))
                         (solve 
                          (lambda (v)
                            (let ((dw^0/dt (ref v 0))
                                  (dw^1/dt (ref v 1)))
                              (let* ((basis-over-mu (basis->basis-over-map mu:N->M S2-spherical-basis))
                                     (1form-basis (basis->1form-basis basis-over-mu))
                                     (vector-basis (basis->vector-basis basis-over-mu))
                                     (Cartan (Christoffel->Cartan G-S2-1))
                                     (transported-vector-over-map 
                                      (basis-components->vector-field
                                       (up (compose (osculating-path (up 'tau 'w^0 dw^0/dt))
                                                    (the-real-line '->coords))
                                           (compose (osculating-path (up 'tau 'w^1 dw^1/dt))
                                                    (the-real-line '->coords)))
                                       vector-basis)))
                                (s:map/r 
                                 (lambda (w)
                                   ((w
                                     (((covariant-derivative Cartan mu:N->M)
                                       U)
                                      transported-vector-over-map))
                                    ((the-real-line '->point) 'tau)))
                                 1form-basis))))
                          (S2-spherical 'dimension)
                          (S2-spherical 'dimension)))
                       '(up
                         (* w^1 (cos (f^theta tau)) (sin (f^theta tau)) ((D f^phi) tau))
                         (/
                          (+ (* -1 w^0 (cos (f^theta tau)) ((D f^phi) tau))
                             (* -1 w^1 ((D f^theta) tau) (cos (f^theta tau))))
                          (sin (f^theta tau))))))
   (test-case
    "Computing parallel transport without the embedding"
    (define-coordinates t the-real-line)
    (define M (make-manifold R^n 2))
    (define M-rect (coordinate-system-at 'rectangular 'origin M))
    (define-coordinates (up theta phi) M-rect)
    (define M-basis (coordinate-system->basis M-rect))
    (define G-S2-1
      (make-Christoffel
       (let ((zero  (lambda (point) 0))) 
         (down (down (up zero zero)
                     (up zero (/ 1 (tan theta))))
               (down (up zero (/ 1 (tan theta)))
                     (up (- (* (sin theta) (cos theta))) zero))))
       M-basis))
    ;;; Parallel transport of vector w over path mu
    (define mu:N->M
      (compose (M-rect '->point)
               (up (literal-function 'mu^theta)
                   (literal-function 'mu^phi))
               (the-real-line '->coords)))
    (define basis-over-mu (basis->basis-over-map mu:N->M M-basis))
    (define w (basis-components->vector-field
               (up (compose (literal-function 'w^0)
                            (the-real-line '->coords))
                   (compose (literal-function 'w^1)
                            (the-real-line '->coords)))
               (basis->vector-basis basis-over-mu)))
    (check-simplified? (let ((Cartan (Christoffel->Cartan G-S2-1)))
                         (s:map/r 
                          (lambda (omega)
                            ((omega
                              (((covariant-derivative Cartan mu:N->M) d/dt) w))
                             ((the-real-line '->point) 'tau)))
                          (basis->1form-basis basis-over-mu)))
                       '(up
                         (+ (* -1 (w^1 tau) ((D mu^phi) tau) (cos (mu^theta tau)) (sin (mu^theta tau)))
                            ((D w^0) tau))
                         (/
                          (+ (* (w^1 tau) (cos (mu^theta tau)) ((D mu^theta) tau))
                             (* (w^0 tau) ((D mu^phi) tau) (cos (mu^theta tau)))
                             (* ((D w^1) tau) (sin (mu^theta tau))))
                          (sin (mu^theta tau)))))
    ;;; These are the equations of the coordinates of a vector being
    ;;; parallel transported along the path defined by f.
    )
   (test-case
    "big"
    ;;; To integrate these equations of the coordinates of the vector
    ;;; being transported along a path (mu^theta(tau), mu^phi(tau)), defined
    ;;; by differential equations we need to make a state space that
    ;;; represents both the path and the coordinates of the vector being
    ;;; transported.  The states are s=(sigma, w)=((theta, phi), (w0, w1))
    ;;; and the differential equations for the path are Dsigma(tau) =
    ;;; b(sigma(tau)).  The differential equations for the coordinates of
    ;;; the vector are driven by this path.

    ;;; To represent these states we make a new manifold with 4
    ;;; coordinates.  The first two coordinates are tha coordinates of the
    ;;; path.  The second two coordinates are the components of the vector
    ;;; to be transported, relative to the coordinate directions in the
    ;;; original manifold.  The right-hand side of the composite
    ;;; differential equation is a vector field on this manifold.
    (define R4 (make-manifold R^n 4))
    (define states (coordinate-system-at 'rectangular 'origin R4))
    (define-coordinates (up theta phi w0 w1) states)
    (define initial-state-d/dphi ((states '->point) (up 'theta0 'phi0 0 1)))
    (define initial-state-d/dtheta ((states '->point) (up 'theta0 'phi0 1 0)))
    ;;; Assuming that the paths are integral curves of a vector field v,
    ;;; we supply the vector field:
    (define (G v)
      (let ((alphadot (dtheta v)) (betadot (dphi v)))
        (+ v
           (* (compose (* sin cos) theta) betadot w1 d/dw0)
           (* -1
              (compose (/ cos sin) theta)
              (+ (* w0 betadot) (* w1 alphadot))
              d/dw1))))
    (define Gu (G d/dtheta))
    (define Gv (G d/dphi))
    (define (initial-state initial-coords w)
      (let ((theta0 (ref initial-coords 0))
            (phi0 (ref initial-coords 1)))
        (let ((dummy
               ((states '->point)
                (up theta0 phi0 'foo 'bar))))
          ((states '->point)
           (up theta0 phi0
               ((dw0 w) dummy)
               ((dw1 w) dummy))))))
    (check-simplified? ((dw0 (commutator Gu Gv))
                        (initial-state (up 'theta0 'phi0) d/dw1))
                       '(* -1 (expt (sin theta0) 2)))
    (check-simplified? ((dw1 (commutator Gu Gv))
                        (initial-state (up 'theta0 'phi0) d/dw0))
                       1)
    ;;; Gee, this gets the right answer.
    )
   (test-case
    "last"
    ;;; To integrate these equations of the coordinates of the vector
    ;;; being transported along a path (mu^theta(tau), mu^phi(tau)), defined
    ;;; by differential equations we need to make a state space that
    ;;; represents both the path and the coordinates of the vector being
    ;;; transported.  The states are s=(sigma, w)=((theta, phi), (w0, w1))
    ;;; and the differential equations for the path are Dsigma(tau) =
    ;;; b(sigma(tau)).  The differential equations for the coordinates of
    ;;; the vector are driven by this path.
    ;;; To represent these states we make a new manifold with 4
    ;;; coordinates.  The first two coordinates are tha coordinates of the
    ;;; path.  The second two coordinates are the components of the vector
    ;;; to be transported, relative to the coordinate directions in the
    ;;; original manifold.  The right-hand side of the composite
    ;;; differential equation is a vector field on this manifold.
    (define M (make-manifold R^n 2))
    (define M-rect (coordinate-system-at 'rectangular 'origin M))
    (define-coordinates (up theta phi) M-rect)
    (define R4 (make-manifold R^n 4))
    (define states (coordinate-system-at 'rectangular 'origin R4))
    (define-coordinates (up Theta Phi w0 w1) states)
    ;;; Assuming that the paths are integral curves of a vector field v,
    ;;; we supply the vector field:
    (define (G v)
      (let ((alphadot (dTheta v)) (betadot (dPhi v)))
        (+ v
           (* (compose (* sin cos) Theta) betadot w1 d/dw0)
           (* -1
              (compose (/ cos sin) Theta)
              (+ (* w0 betadot) (* w1 alphadot))
              d/dw1))))
    (define Gu (G d/dTheta))
    (define Gv (G d/dPhi))
    (define (initial-state initial-coords w)
      (let ((Theta0 (ref initial-coords 0))
            (Phi0 (ref initial-coords 1)))
        (let ((m ((M-rect '->point) (up Theta0 Phi0))))
          ((states '->point)
           (up Theta0 Phi0
               ((dtheta w) m) ((dphi w) m))))))
    (check-simplified? ((dw0 (commutator Gu Gv))
                        (initial-state (up 'Theta0 'Phi0) d/dphi))
                       '(* -1 (expt (sin Theta0) 2)))
    (check-simplified? ((dw1 (commutator Gu Gv))
                        (initial-state (up 'Theta0 'Phi0) d/dtheta))
                       1)
    ;;; Gee, this gets the right answer.
    )
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))
