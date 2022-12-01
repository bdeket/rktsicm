#lang s-exp "../../main.rkt"

(require rackunit
         "../helper.rkt")

(rename-part 'derivative 'D)

(define the-tests
  (test-suite
   "mechanics/Lagrangian"
   (test-case
    "free-particle"
    (define ((L-free-particle mass) local)
      (let ((v (velocity local)))
        (* 1/2 mass (square v))))
    (check-simplified? ((L-free-particle 'm)
                        (->local 't
                                 (coordinate-tuple 'x 'y 'z)
                                 (velocity-tuple 'xdot 'ydot 'zdot)))
                       '(+ (* 1/2 m (expt xdot 2))
                           (* 1/2 m (expt ydot 2))
                           (* 1/2 m (expt zdot 2))))
    (check-simplified? ((compose
                         (L-free-particle 'm)
                         (Gamma (coordinate-tuple (literal-function 'x)
                                                  (literal-function 'y)
                                                  (literal-function 'z))))
                        't)
                       '(+ (* 1/2 (expt ((D x) t) 2) m)
                           (* 1/2 (expt ((D y) t) 2) m)
                           (* 1/2 (expt ((D z) t) 2) m)))
    (define (test-path t)
      (coordinate-tuple (+ (* 'a t) 'a0)
                        (+ (* 'b t) 'b0)
                        (+ (* 'c t) 'c0)))
    (check-simplified? (((Lagrange-equations (L-free-particle 'm))
                         test-path)
                        't)
                       '(down 0 0 0))
    (check-simplified? (((Lagrange-equations (L-free-particle 'm))
                         (literal-function 'x))
                        't)
                       '(* m (((expt D 2) x) t))))
   (test-case
    "harmonic"
;;; For example, consider the Harmonic oscillator with
;;;  spring constant, k, and mass, m.
    (define ((L-harmonic m k) local)
      (let ((q (coordinate local)) 
            (v (velocity local)))
        (- (* 1/2 m (square v))
           (* 1/2 k (square q)))))
    (check-simplified? (((Lagrange-equations (L-harmonic 'm 'k))
                         (literal-function 'x))
                        't)
                       '(+ (* k (x t)) (* m (((expt D 2) x) t))))
    (check-simplified? (((Lagrange-equations (L-harmonic 'm 'k))
                         (lambda (t) (* 'a (cos (+ (* 'omega t) 'phi)))))
                        't)
                       '(+ (* a k (cos (+ (* omega t) phi)))
                           (* -1 a m (expt omega 2) (cos (+ (* omega t) phi)))))
    (check-simplified? (((Lagrange-equations-1 (L-harmonic 'm 'k))
                         (coordinate-tuple (literal-function 'x)
                                           (literal-function 'y))
                         (velocity-tuple (literal-function 'v_x)
                                         (literal-function 'v_y)))
                        't)
                       '(up 0
                            (up (+ ((D x) t) (* -1 (v_x t))) (+ ((D y) t) (* -1 (v_y t))))
                            (up (+ (/ (* k (x t)) m) ((D v_x) t)) (+ (/ (* k (y t)) m) ((D v_y) t))))))
   (test-case
    "uniform-acceleration"
    (define ((L-uniform-acceleration m g) local)
      (let ((q (coordinate local))
            (v (velocity local)))
        (let ((y (ref q 1)))
          (- (* 1/2 m (square v)) (* m g y)))))
    (check-simplified? (((Lagrange-equations
                          (L-uniform-acceleration 'm 'g))
                         (coordinate-tuple (literal-function 'x)
                                           (literal-function 'y)))
                        't)
                       '(down (* m (((expt D 2) x) t))
                              (+ (* g m) (* m (((expt D 2) y) t))))))
   (test-case
    "central-rectangular"
    (define ((L-central-rectangular m V) local)
      (let ((q (coordinate local))
            (v (velocity local)))
        (- (* 1/2 m (square v))
           (V (sqrt (square q))))))
    (check-simplified? (((Lagrange-equations 
                          (L-central-rectangular 'm (literal-function 'V)))
                         (coordinate-tuple (literal-function 'x) (literal-function 'y)))
                        't)
                       '(down
                         (+ (* m (((expt D 2) x) t))
                            (/ (* ((D V) (sqrt (+ (expt (x t) 2) (expt (y t) 2)))) (x t))
                               (sqrt (+ (expt (x t) 2) (expt (y t) 2)))))
                         (+ (* m (((expt D 2) y) t))
                            (/ (* ((D V) (sqrt (+ (expt (x t) 2) (expt (y t) 2)))) (y t))
                               (sqrt (+ (expt (x t) 2) (expt (y t) 2))))))))
   (test-case
    "central-polar"
    ;;; Consider planar motion in a central force field, with an arbitrary
    ;;; potential, U, depending only on the radius.  The generalized
    ;;; coordinates are polar. 
    (define ((L-central-polar m V) local)
      (let ((q (coordinate local))
            (qdot (velocity local)))
        (let ((r (ref q 0))
              (phi (ref q 1))
              (rdot (ref qdot 0))
              (phidot (ref qdot 1)))
          (- (* 1/2 m
                (+ (square rdot)
                   (square (* r phidot))) )
             (V r)))))
    (check-simplified? (((Lagrange-equations
                          (L-central-polar 'm (literal-function 'V)))
                         (coordinate-tuple (literal-function 'r)
                                           (literal-function 'phi)))
                        't)
                       '(down
                         (+ (* -1 m (r t) (expt ((D phi) t) 2))
                            (* m (((expt D 2) r) t))
                            ((D V) (r t)))
                         (+ (* 2 m ((D r) t) (r t) ((D phi) t))
                            (* m (((expt D 2) phi) t) (expt (r t) 2)))))
    ;;; For example, on a specified trajectory, we can compute the energy,
    ;;; which turns out to be T+V.
    (check-simplified? ((compose
                          (Lagrangian->energy (L-central-polar 'm (literal-function 'U)))
                          (Gamma 
                           (coordinate-tuple (literal-function 'r) (literal-function 'phi))))
                         't)
                       '(+ (* 1/2 m (expt (r t) 2) (expt ((D phi) t) 2))
                           (* 1/2 m (expt ((D r) t) 2))
                           (U (r t))))
    ;;; In fact, we can see how the energy is conserved:

    (check-simplified? (((Lagrangian->power-loss (L-central-polar 'm (literal-function 'U)))
                         (coordinate-tuple (literal-function 'r) (literal-function 'phi)))
                        't)
                       '(+ (* m (((expt D 2) phi) t) ((D phi) t) (expt (r t) 2))
                           (* m (expt ((D phi) t) 2) (r t) ((D r) t))
                           (* m (((expt D 2) r) t) ((D r) t))
                           (* ((D U) (r t)) ((D r) t))))
    ;;; This last expression is (nontrivially!) zero on any trajectory
    ;;; which satisfies Lagrange's equations.
    )
   (test-case
    "coupled harmonic oscillator"
    (define ((L-coupled-harmonic m k) state)
      (let ((q (coordinate state))
            (qdot (velocity state)))
        (- (* 1/2 qdot m qdot)
           (* 1/2 q k q))))
    (check-simplified? (((Lagrange-equations
                          (L-coupled-harmonic (down (down 'm_1 0) (down 0 'm_2))
                                              (down (down 'k_1 'c) (down 'c 'k_2))))
                         (coordinate-tuple (literal-function 'x)
                                           (literal-function 'y)))
                        't)
                       '(down (+ (* c (y t)) (* k_1 (x t)) (* m_1 (((expt D 2) x) t)))
                              (+ (* c (x t)) (* k_2 (y t)) (* m_2 (((expt D 2) y) t))))))
   (test-case
    "Pendulum"
    ;;; Pendulum of mass m2 and length b, hanging from a support of mass
    ;;; m1 that is free to move horizontally (from Groesberg, Advanced
    ;;; Mechanics, p. 72) 
    (define ((L-sliding-pend m1 m2 b g) state)
      (let ((q (coordinate state))
            (qdot (velocity state)))
        (let* ((x (ref q 0))
               (xdot (ref qdot 0))
               (theta (ref q 1))
               (thetadot (ref qdot 1))
               (rel-pend-vel
                (* b thetadot (velocity-tuple (cos theta) (sin theta))))
               (pend-vel (+ rel-pend-vel (velocity-tuple xdot 0)))
               (Tpend (* 1/2 m2 (square pend-vel)))
               (Tsupport (* 1/2 m1 (square xdot)))
               (V (- (* m2 g b (cos theta)))))
          (+ Tpend Tsupport (- V)))))
    (check-simplified? (((Lagrange-equations (L-sliding-pend 'm_1 'm_2 'b 'g))
                         (coordinate-tuple (literal-function 'x)
                                           (literal-function 'theta)))
                        't)
                       '(down
                         (+ (* -1 b m_2 (sin (theta t)) (expt ((D theta) t) 2))
                            (* b m_2 (((expt D 2) theta) t) (cos (theta t)))
                            (* m_1 (((expt D 2) x) t))
                            (* m_2 (((expt D 2) x) t)))
                         (+ (* (expt b 2) m_2 (((expt D 2) theta) t))
                            (* b g m_2 (sin (theta t)))
                            (* b m_2 (((expt D 2) x) t) (cos (theta t)))))))
   (test-case
    "nicer pendulum"
    ;;; Nicer treatment
    (define ((F-sliding-pend l) state)
      (let ((q (coordinate state)))
        (let ((x (ref q 0))
              (theta (ref q 1)))
          (up (up x 0)
              (up (+ x (* l (sin theta)))
                  (* -1 l (cos theta)))))))
    (define ((2-free m1 m2 g) state)
      (let ((v1 (ref (velocity state) 0))
            (v2 (ref (velocity state) 1))
            (h1 (ref (coordinate state) 0 1))
            (h2 (ref (coordinate state) 1 1)))
        (- (+ (* 1/2 m1 (square v1))
              (* 1/2 m2 (square v2)))
           (+ (* m1 g h1)
              (* m2 g h2)))))
    (define (L-sliding-pend m1 m2 l g)
      (compose (2-free m1 m2 g)
               (F->C (F-sliding-pend l))))
    (check-simplified? (((Lagrange-equations
                          (L-sliding-pend 'm_1 'm_2 'b 'g))
                         (up (literal-function 'x)
                             (literal-function 'theta)))
                        't)
                       '(down
                         (+ (* -1 b m_2 (sin (theta t)) (expt ((D theta) t) 2))
                            (* b m_2 (((expt D 2) theta) t) (cos (theta t)))
                            (* m_1 (((expt D 2) x) t))
                            (* m_2 (((expt D 2) x) t)))
                         (+ (* (expt b 2) m_2 (((expt D 2) theta) t))
                            (* b g m_2 (sin (theta t)))
                            (* b m_2 (cos (theta t)) (((expt D 2) x) t)))))
    (check-simplified? ((Lagrangian->acceleration (L-sliding-pend 'm_1 'm_2 'b 'g))
                        (->local 't
                                 (coordinate-tuple 'x 'theta)
                                 (velocity-tuple 'xdot 'thetadot)))
                       '(up
                         (+
                          (/ (* b m_2 (expt thetadot 2) (sin theta))
                             (+ (* m_2 (expt (sin theta) 2)) m_1))
                          (/ (* g m_2 (sin theta) (cos theta))
                             (+ (* m_2 (expt (sin theta) 2)) m_1)))
                         (+
                          (/ (* -1 m_2 (expt thetadot 2) (sin theta) (cos theta))
                             (+ (* m_2 (expt (sin theta) 2)) m_1))
                          (/ (* -1 g m_1 (sin theta))
                             (+ (* b m_2 (expt (sin theta) 2)) (* b m_1)))
                          (/ (* -1 g m_2 (sin theta))
                             (+ (* b m_2 (expt (sin theta) 2)) (* b m_1)))))))
   (test-case
    "pendulum+Rayleigh"
    (define ((L-pendulum g m l) state)
      (let ((theta (coordinate state))
            (thetadot (velocity state)))
        (+ (* 1/2 m (square (* l thetadot)))
           (* g m l (cos theta)))))
    (define ((Rayleigh-dissipation k) state)
      (let ((qdot (velocity state)))
        (* qdot k qdot)))
    (check-simplified? (((Lagrange-equations (L-pendulum 'g 'm 'l)
                                             (Rayleigh-dissipation 'k))
                         (literal-function 'theta))
                        't)
                       '(+ (* 2 k ((D theta) t))
                           (* g l m (sin (theta t)))
                           (* (expt l 2) m (((expt D 2) theta) t))))
    (check-simplified? ((Lagrangian->state-derivative (L-pendulum 'g 'm 'l)
                                                      (Rayleigh-dissipation 'k))
                        (up 't 'theta 'thetadot))
                       '(up 1
                            thetadot
                            (+ (/ (* -1 g (sin theta)) l)
                               (/ (* -2 k thetadot) (* (expt l 2) m))))))
   (test-case
    "two-particle"
    ;;; Can group coordinates.  Procedures don't care.
    (define ((L-two-particle m1 m2) local)
      (let ((x (coordinate local))
            (v (velocity local))
            (V (literal-function 'V (-> (X (^ Real 2) (^ Real 2)) Real))))
        (let ((x1 (ref x 0)) (x2 (ref x 1))
                             (v1 (ref v 0)) (v2 (ref v 1)))
          (- (+ (* 1/2 m1 (square v1))
                (* 1/2 m2 (square v2)))
             (V x1 x2)))))
    (check-simplified? (((Lagrange-equations (L-two-particle 'm_1 'm_2))
                         (coordinate-tuple
                          (coordinate-tuple (literal-function 'x_1) (literal-function 'y_1))
                          (coordinate-tuple (literal-function 'x_2) (literal-function 'y_2))))
                        't)
                       '(down
                         (down
                          (+ (* m_1 (((expt D 2) x_1) t))
                             (((partial 0 0) V) (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t))))
                          (+ (* m_1 (((expt D 2) y_1) t))
                             (((partial 0 1) V) (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t)))))
                         (down
                          (+ (* m_2 (((expt D 2) x_2) t))
                             (((partial 1 0) V) (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t))))
                          (+ (* m_2 (((expt D 2) y_2) t))
                             (((partial 1 1) V) (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t))))))))
   (test-case
    "T3-spherical"
    ;;; Note, this can be implemented in terms of T-CURVILINEAR.
    (define ((T3-spherical m) local)
      (let ((t (time local))
            (q (coordinate local))
            (qdot (velocity local)))
        (let ((r (ref q 0))
              (theta (ref q 1))
              (phi (ref q 2))
              (rdot (ref qdot 0))
              (thetadot (ref qdot 1))
              (phidot (ref qdot 2)))
          (* 1/2 m
             (+ (square rdot)
                (square (* r thetadot))
                (square (* r (sin theta) phidot)))))))
    (define (L3-central m Vr)
      (define (Vs local)
        (let ((r (ref (coordinate local) 0)))
          (Vr r)))
      (- (T3-spherical m) Vs))
    (check-simplified? (((partial 1) (L3-central 'm (literal-function 'V)))
                        (->local 't
                                 (coordinate-tuple 'r 'theta 'phi)
                                 (velocity-tuple 'rdot 'thetadot 'phidot)))
                       '(down
                         (+ (* m r (expt phidot 2) (expt (sin theta) 2))
                            (* m r (expt thetadot 2))
                            (* -1 ((D V) r)))
                         (* m (expt r 2) (expt phidot 2) (cos theta) (sin theta))
                         0))
    (check-simplified? (((partial 2) (L3-central 'm (literal-function 'V)))
                        (->local 't 
                                 (coordinate-tuple 'r 'theta 'phi)
                                 (velocity-tuple 'rdot 'thetadot 'phidot)))
                       '(down (* m rdot)
                              (* m (expt r 2) thetadot)
                              (* m (expt r 2) phidot (expt (sin theta) 2)))))

   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))