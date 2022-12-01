#lang s-exp "../../main.rkt"

(require rackunit
         "../helper.rkt")

(rename-part 'derivative 'D)

(define the-tests
  (test-suite
   "mechanics/rigid"
   (check-simplified? ((Euler->omega-body
                        (up (literal-function 'theta)
                            (literal-function 'phi)
                            (literal-function 'psi)))
                       't)
                      '(matrix-by-rows
                        (list (+ (* (sin (theta t)) (sin (psi t)) ((D phi) t))
                                 (* ((D theta) t) (cos (psi t)))))
                        (list (+ (* (sin (theta t)) (cos (psi t)) ((D phi) t))
                                 (* -1 ((D theta) t) (sin (psi t)))))
                        (list (+ (* (cos (theta t)) ((D phi) t))
                                 ((D psi) t)))))
   (check-simplified? (((M-of-q->omega-body-of-t Euler->M)
                        (up (literal-function 'theta)
                            (literal-function 'phi)
                            (literal-function 'psi)))
                       't)
                      '(matrix-by-rows
                        (list (+ (* (sin (theta t)) (sin (psi t)) ((D phi) t))
                                 (* ((D theta) t) (cos (psi t)))))
                        (list (+ (* (sin (theta t)) (cos (psi t)) ((D phi) t))
                                 (* -1 ((D theta) t) (sin (psi t)))))
                        (list (+ (* (cos (theta t)) ((D phi) t))
                                 ((D psi) t)))))
   (check-simplified? ((M->omega-body Euler->M)
                       (up 't 
                           (up 'theta 'phi 'psi)
                           (up 'thetadot 'phidot 'psidot)))
                      '(matrix-by-rows
                        (list (+ (* phidot (sin psi) (sin theta)) (* thetadot (cos psi))))
                        (list (+ (* phidot (cos psi) (sin theta)) (* -1 thetadot (sin psi))))
                        (list (+ (* phidot (cos theta)) psidot))))
   (test-case
    "Euler-state"
    (define an-Euler-state
      (up 't
          (up 'theta 'phi 'psi)
          (up 'thetadot 'phidot 'psidot)))
    (check-simplified? (ref
                        (((partial 2) (T-body-Euler 'A 'B 'C))
                         an-Euler-state)
                        1)
                       '(+ (* A phidot (expt (sin psi) 2) (expt (sin theta) 2))
                           (* B phidot (expt (cos psi) 2) (expt (sin theta) 2))
                           (* A thetadot (cos psi) (sin psi) (sin theta))
                           (* -1 B thetadot (cos psi) (sin psi) (sin theta))
                           (* C phidot (expt (cos theta) 2))
                           (* C psidot (cos theta))))
    (check-simplified? (- (ref ((L-space-Euler 'A 'B 'C) an-Euler-state) 2)        ;$L_z$
                          (ref (((partial 2) (T-body-Euler 'A 'B 'C)) an-Euler-state) 1)  ;$p_\phi$
                          )
                       0)
    (check-simplified? (determinant
                        (((compose (partial 2) (partial 2)) 
                          (T-body-Euler 'A 'B 'C))
                         an-Euler-state))
                       '(* A B C (expt (sin theta) 2))))
   (test-case
    "rigid"
    (define (rigid-sysder A B C)
      (Lagrangian->state-derivative (T-body-Euler A B C)))
    (set-ode-integration-method! 'bulirsch-stoer)
    (check-within (let ((A 1.) (B (sqrt 2.)) (C 2.)
                               (state0 (up 0.0
                                           (up 1. 0. 0.)
                                           (up 0.1 0.1 0.1))))
                    (let ((L0 ((L-space-Euler A B C) state0))
                          (E0 ((T-body-Euler A B C) state0)))
                      ((evolve rigid-sysder A B C)
                       state0
                       void
                       0.1
                       100.0
                       1.0e-12)))
                  (up 99.99999999999864
                      (up .6319896958334494 1.3610271540875034 17.437900484737938)
                      (up -.12343716197181527 .09016109524808046 .07567921658605782))
                  1e-10))
   (check-simplified? ((T-body-Euler 'A 'A 'C) 
                       (up 't 
                           (up 'theta 'phi 'psi)
                           (up 'thetadot 'phidot 'psidot)))
                      '(+ (* 1/2 A (expt phidot 2) (expt (sin theta) 2))
                          (* 1/2 C (expt phidot 2) (expt (cos theta) 2))
                          (* C phidot psidot (cos theta))
                          (* 1/2 A (expt thetadot 2))
                          (* 1/2 C (expt psidot 2))))
   (check-simplified? (let ((Euler (up 'theta 'phi 'psi))
                            (v (up 'x 'y 'z)))
                        (let ((M (Euler->M Euler)))
                          (- (* (3vector-components->antisymmetric (* M v))
                                M)
                             (* M
                                (3vector-components->antisymmetric v)))))
                      '(matrix-by-rows (list 0 0 0) (list 0 0 0) (list 0 0 0)))
   (check-simplified? (let ((Euler (up (literal-function 'theta)
                                       (literal-function 'phi)
                                       (literal-function 'psi))))
                        (antisymmetric->column-matrix 
                         (* (transpose ((Euler->M Euler) 't))
                            ((D (Euler->M Euler)) 't))))
                      '(matrix-by-rows
                        (list
                         (+ (* ((D phi) t) (sin (psi t)) (sin (theta t)))
                            (* ((D theta) t) (cos (psi t)))))
                        (list
                         (+ (* ((D phi) t) (sin (theta t)) (cos (psi t)))
                            (* -1 (sin (psi t)) ((D theta) t))))
                        (list (+ (* (cos (theta t)) ((D phi) t)) ((D psi) t)))))
   (test-case
    "Veff"
    (define ((V_eff p A C gMR) theta)
      (+ (/ (square p) (* 2 C))
         (* (/ (square p) (* 2 A))
            (square (tan (/ theta 2))))
         (* gMR (cos theta))))
    (check-simplified? (((square derivative) (V_eff 'p_c 'A 'C 'gMR)) 0)
                       '(+ (* -1 gMR) (/ (* 1/4 (expt p_c 2)) A)))
    (check-= (* (/ 60 2pi) (/ 7.734804457773965e-3 6.6e-5)) 1119.1203302763215 1e-10))
   (test-case
    "qw-sysder"
    (define (qw-sysder A B C)
      (let ((B-C/A (/ (- B C) A))
            (C-A/B (/ (- C A) B))
            (A-B/C (/ (- A B) C)))
        (define (the-deriv qw-state)
          (let ((t (time qw-state))
                (q (coordinates qw-state))
                (omega-body (ref qw-state 2)))
            (let ((omega^a (ref omega-body 0))
                  (omega^b (ref omega-body 1))
                  (omega^c (ref omega-body 2)))
              (let ((tdot 1)
                    (qdot      ;driven quaternion
                     (* -1/2
                        (+ (* omega^a q:i)
                           (* omega^b q:j)
                           (* omega^c q:k))
                        q))
                    (omegadot  ;Euler's equations
                     (up (* B-C/A omega^b omega^c)
                         (* C-A/B omega^c omega^a)
                         (* A-B/C omega^a omega^b))))
                (up tdot qdot omegadot)))))
        the-deriv))
    (set-ode-integration-method! 'bulirsch-stoer)
    (check-within (let* ((A 1.) (B (sqrt 2.)) (C 2.)   ; moments of inertia
                                (Euler-state (up 0.0           ; initial state
                                                 (up 1. 0. 0.)
                                                 (up 0.1 0.1 0.1)))
                                (M (Euler->M (coordinates Euler-state)))
                                (q (quaternion->vector (rotation-matrix->quaternion M)))
                                (qw-state0 
                                 (up (time Euler-state)
                                     q
                                     (Euler-state->omega-body Euler-state))))
                    (let ((L0 ((qw-state->L-space A B C) qw-state0))
                          (E0 ((T-body A B C) (ref qw-state0 2))))
                      ((evolve qw-sysder A B C)
                       qw-state0
                       void
                       0.1                  ; step between plotted points
                       100.0                ; final time
                       1.0e-12)))
                  #(100.0
                    #(-0.9501831654548668 -0.05699715799969957 -0.3054905540186666 0.024058210063846806)
                    #(-0.07215083472578741 -0.11343682989477975 0.14842602905083652))
                  1e-15))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))