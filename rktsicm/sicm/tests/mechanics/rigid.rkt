#lang s-exp "../../main.rkt"

(require rackunit
         "../../mechanics/rigid.rkt"
         "../helper+scm.rkt")

(rename-part 'derivative 'D)

(provide the-tests)
(define (make-ϕ #:x [x #f] #:y [y #f] #:z [z #f])
  (define (>> x v) (if (eq? x #f) (literal-function v) (if (numerical-quantity? x) (constant x) x)))
  (up (>> x 'ϕ0) (>> y 'ϕ1) (>> z 'ϕ2)))
(define the-tests
  (test-suite
   "mechanics/rigid"
   (test-case
    "antisymmetric"
    (check-true  (m:antisymmetric? (matrix-by-rows '(0 1 2) '(-1 0 -3) '(-2 3 0))))
    (check-false (m:antisymmetric? (matrix-by-rows '(0 1 2) '(-1 0 -3) '(-2 3 1))))
    (check-true  (m:antisymmetric? (matrix-by-rows '(0 1) '(-1 0))))
    (check-false (m:antisymmetric? (matrix-by-rows '(0 0) '(0 1))))
    ;;TODO;; antisymetric is only for 3×3 matrices
    (check-equal? (antisymmetric->column-matrix (matrix-by-rows '(0 1 2) '(-1 0 -3) '(-2 3 0)))
                  (matrix-by-cols '(3 2 -1)))
    (check-exn #px"assertion failed: \\(m:antisymmetric\\? A\\)"
               (λ () (antisymmetric->column-matrix  (matrix-by-rows '(0 1 2) '(-1 0 -3) '(-2 3 1)))))
    (check-exn #px"assertion failed: \\(rkt:= \\(m:dimension A\\) 3\\)"
               (λ () (antisymmetric->column-matrix (matrix-by-rows '(0 1) '(-1 0)))))
    (check-simplified? (3vector-components->antisymmetric (up 'a 'b 'c))
                       (matrix-by-rows '(0 (- c) b) '(c 0 (- a)) '((- b) a 0))))
   (test-case
    "Euler->M"
    ;; Euler is (intrinsic) euler angles in (x2 z1 z3) format, M is the rotation matrix
    ;; ie first rotate around z1 then x2 then z3 (of the rotated axes)
    (check-simplified? (Euler->M (up 'a 0 0))
                       (matrix-by-rows '(1 0 0) '(0 (cos a) (* -1 (sin a))) '(0 (sin a) (cos a))))
    (check-simplified? (Euler->M (up 0 'b 0))
                       (matrix-by-rows '((cos b) (* -1 (sin b)) 0) '((sin b) (cos b) 0) '(0 0 1)))
    (check-simplified? (Euler->M (up 0 'a 0))
                       (Euler->M (up 0 0 'a)))
    (check-within (* (Euler->M (up :pi/2 0 0)) (up 1 1 1)) (up 1 -1 1) 1e-12)
    (check-within (* (Euler->M (up 0 :pi/2 0)) (up 1 1 1)) (up -1 1 1) 1e-12)
    (check-within (* (Euler->M (up 0 0 :pi/2)) (up 1 1 1)) (up -1 1 1) 1e-12)
    (check-within (* (Euler->M (up :pi/2 :pi/2 0)) (up 1 1 1)) (up 1 1 1) 1e-12)
    (check-within (* (Euler->M (up :pi/2 0 :pi/2)) (up 1 1 1)) (up -1 -1 1) 1e-12)
    (check-within (* (Euler->M (up :pi/2 :pi/2 :pi/2)) (up 1 1 1)) (up 1 -1 1) 1e-12))
   (test-case
    "Euler->omega"
    ;; omega = angular speed
    (check-simplified? ((Euler->omega (make-ϕ #:y 0 #:z 0)) 't)
                       (matrix-by-cols '(((D ϕ0) t) 0 0)))
    (check-simplified? ((Euler->omega (make-ϕ #:x 0 #:z 0)) 't)
                       (matrix-by-cols '(0 0 ((D ϕ1) t))))
    (check-simplified? ((Euler->omega (make-ϕ #:x 0 #:y 0)) 't)
                       (matrix-by-cols '(0 0 ((D ϕ2) t))))
    (check-simplified? ((Euler->omega (make-ϕ #:z 0)) 't)
                       (matrix-by-cols '((* ((D ϕ0) t) (cos (ϕ1 t))) (* ((D ϕ0) t) (sin (ϕ1 t))) ((D ϕ1) t))))
    (check-simplified? ((Euler->omega (make-ϕ #:y 0)) 't)
                       (matrix-by-cols '(((D ϕ0) t) (* -1 ((D ϕ2) t) (sin (ϕ0 t))) (* ((D ϕ2) t) (cos (ϕ0 t)))))))
   (test-case
    "Euler->body"
    (check-simplified? ((Euler->omega-body (make-ϕ #:y 0 #:z 0)) 't)
                       (matrix-by-cols '(((D ϕ0) t) 0 0)))
    (check-simplified? ((Euler->omega-body (make-ϕ #:x 0 #:z 0)) 't)
                       (matrix-by-cols '(0 0 ((D ϕ1) t))))
    (check-simplified? ((Euler->omega-body (make-ϕ #:x 0 #:y 0)) 't)
                       (matrix-by-cols '(0 0 ((D ϕ2) t))))
    (check-simplified? ((Euler->omega-body (make-ϕ #:z 0)) 't)
                       (matrix-by-cols '(((D ϕ0) t) (* ((D ϕ1) t) (sin (ϕ0 t))) (* ((D ϕ1) t) (cos (ϕ0 t))))))
    (check-simplified? ((Euler->omega-body (make-ϕ #:y 0)) 't)
                       (matrix-by-cols '((* ((D ϕ0) t) (cos (ϕ2 t))) (* -1 ((D ϕ0) t) (sin (ϕ2 t))) ((D ϕ2) t)))))
   (test-case
    "M-of-q->"
    ;; M-of-q is path to rotation matrix, for example Eueler->M on euler-angle-path
    (check-simplified? (((M-of-q->omega-of-t Euler->M) (make-ϕ #:y 0 #:z 0)) 't)
                       (matrix-by-cols '(((D ϕ0) t) 0 0)))
    (check-simplified? (((M-of-q->omega-of-t Euler->M) (make-ϕ #:z 0)) 't)
                       (matrix-by-cols '((* ((D ϕ0) t) (cos (ϕ1 t))) (* ((D ϕ0) t) (sin (ϕ1 t))) ((D ϕ1) t))))
    (check-simplified? (((M-of-q->omega-body-of-t Euler->M) (make-ϕ #:y 0 #:z 0)) 't)
                       (matrix-by-cols '(((D ϕ0) t) 0 0)))
    (check-simplified? (((M-of-q->omega-body-of-t Euler->M) (make-ϕ #:z 0)) 't)
                       (matrix-by-cols '(((D ϕ0) t) (* ((D ϕ1) t) (sin (ϕ0 t))) (* ((D ϕ1) t) (cos (ϕ0 t)))))))
   (test-case
    "M->"
    (check-simplified? ((M->omega Euler->M) (up 't (up 'theta 'phi 'psi) (up 'thetadot 'phidot 'psidot)))
                       (matrix-by-rows '((+ (* psidot (sin theta) (sin phi)) (* thetadot (cos phi))))
                                       '((+ (* -1 psidot (cos phi) (sin theta)) (* thetadot (sin phi))))
                                       '((+ (* psidot (cos theta)) phidot))))
    (check-simplified? ((M->omega-body Euler->M) (up 't (up 'theta 'phi 'psi) (up 'thetadot 'phidot 'psidot)))
                       (matrix-by-rows '((+ (* phidot (sin theta) (sin psi)) (* thetadot (cos psi))))
                                       '((+ (* phidot (sin theta) (cos psi)) (* -1 thetadot (sin psi))))
                                       '((+ (* phidot (cos theta)) psidot)))))
   (test-case
    "_-body"
    ;; kinetic energy with ABC principal moments and omega on principal axes
    (check-simplified? (((T-body 'A 'B 'C) (Euler->omega-body (make-ϕ #:x 0))) 't)
                       '(+ (* 1/2 C (expt ((D ϕ1) t) 2))
                           (* C ((D ϕ1) t) ((D ϕ2) t))
                           (* 1/2 C (expt ((D ϕ2) t) 2))))
    ;; angular momentum
    (check-simplified? (((L-body 'A 'B 'C) (Euler->omega-body (make-ϕ #:x 0))) 't)
                       '(down 0 0 (+ (* C ((D ϕ1) t)) (* C ((D ϕ2) t)))))
    (define M (literal-matrix 'm 3 3))
    (check-simplified? ((((L-space (literal-matrix 'm 3 3)) 'A 'B 'C) (Euler->omega-body (make-ϕ #:x 0))) 't)
                       '(down (+ (* C m^0_2 ((D ϕ1) t)) (* C m^0_2 ((D ϕ2) t)))
                              (+ (* C m^1_2 ((D ϕ1) t)) (* C m^1_2 ((D ϕ2) t)))
                              (+ (* C m^2_2 ((D ϕ1) t)) (* C m^2_2 ((D ϕ2) t))))))
   (test-case
    "Euler-state->_"
    (define s (up 't (up 'a 'b 'c) (up 'a^ 'b^ 'c^)))
    (check-simplified? (Euler-state->omega-body s)
                       '(up (+ (* b^ (sin a) (sin c)) (* a^ (cos c)))
                            (+ (* b^ (sin a) (cos c)) (* -1 a^ (sin c)))
                            (+ (* b^ (cos a)) c^)))
    (check-simplified? ((T-body-Euler 'A 'B 'C) s)
                       '(+ (* 1/2 A (expt b^ 2) (expt (sin a) 2) (expt (sin c) 2))
                           (* 1/2 B (expt b^ 2) (expt (sin a) 2) (expt (cos c) 2))
                           (* A b^ a^ (sin a) (cos c) (sin c))
                           (* -1 B b^ a^ (sin a) (cos c) (sin c))
                           (* 1/2 A (expt a^ 2) (expt (cos c) 2))
                           (* 1/2 B (expt a^ 2) (expt (sin c) 2))
                           (* 1/2 C (expt b^ 2) (expt (cos a) 2))
                           (* C b^ c^ (cos a))
                           (* 1/2 C (expt c^ 2))))
    (check-eq? T-body-Euler T-rigid-body)
    (check-simplified? ((L-body-Euler 'A 'B 'C) s)
                       '(down (+ (* A b^ (sin a) (sin c)) (* A a^ (cos c)))
                              (+ (* B b^ (sin a) (cos c)) (* -1 B a^ (sin c)))
                              (+ (* C b^ (cos a)) (* C c^))))
    (check-eq? L-body-Euler Euler-state->L-body)
    (check-simplified? ((L-space-Euler 'A 'B 'C) s)
                       '(down (+ (* -1 A b^ (cos a) (sin a) (sin b) (expt (sin c) 2)) (* -1 B b^ (cos a) (sin a) (sin b) (expt (cos c) 2)) (* -1 A a^ (cos a) (sin b) (sin c) (cos c))
                                 (* A b^ (sin a) (cos b) (sin c) (cos c)) (* B a^ (cos a) (sin b) (sin c) (cos c)) (* -1 B b^ (sin a) (cos b) (sin c) (cos c))
                                 (* A a^ (cos b) (expt (cos c) 2)) (* B a^ (cos b) (expt (sin c) 2)) (* C b^ (cos a) (sin a) (sin b)) (* C c^ (sin a) (sin b)))
                              (+ (* A b^ (cos a) (sin a) (cos b) (expt (sin c) 2)) (* B b^ (cos a) (sin a) (cos b) (expt (cos c) 2)) (* A a^ (cos a) (cos b) (sin c) (cos c))
                                 (* A b^ (sin a) (sin b) (sin c) (cos c)) (* -1 B a^ (cos a) (cos b) (sin c) (cos c)) (* -1 B b^ (sin a) (sin b) (sin c) (cos c))
                                 (* A a^ (sin b) (expt (cos c) 2)) (* B a^ (sin b) (expt (sin c) 2)) (* -1 C b^ (cos a) (sin a) (cos b)) (* -1 C c^ (sin a) (cos b)))
                              (+ (* A b^ (expt (sin a) 2) (expt (sin c) 2)) (* B b^ (expt (sin a) 2) (expt (cos c) 2)) (* A a^ (sin a) (sin c) (cos c)) (* -1 B a^ (sin a) (sin c) (cos c)) (* C b^ (expt (cos a) 2)) (* C c^ (cos a)))))
    (check-eq? L-space-Euler Euler-state->L-space))
   (test-case
    "relative error"
    ;; TODO this is not the right place for this definition
    (check-equal? (relative-error 3 1) 2)
    (check-equal? (relative-error 0 1) -1)
    (check-exn #px"Zero reference value -- RELATIVE-ERROR" (λ ()(relative-error 3 0))))
   (test-case
    "quaternion"
    ;; coordinates not given as quaternions but up-tuples
    (define s (up 't (up 'a 'b 'c 'd) (up 'a^ 'b^ 'c^ 'd^)))
    (define qw (up 't (up 'a 'b 'c 'd) (up 'a^ 'b^ 'c^)))
    (check-simplified? (quaternion-state->omega-body s)
                       (up '(/ (+ (* 2 a b^) (* -2 a^ b) (* -2 c d^) (*  2 c^ d)) (+ (expt a 2) (expt b 2) (expt c 2) (expt d 2)))
                           '(/ (+ (* 2 a c^) (* -2 a^ c) (*  2 b d^) (* -2 b^ d)) (+ (expt a 2) (expt b 2) (expt c 2) (expt d 2)))
                           '(/ (+ (* 2 a d^) (* -2 a^ d) (* -2 b c^) (*  2 b^ c)) (+ (expt a 2) (expt b 2) (expt c 2) (expt d 2)))))

    (check-simplified? (quaternion-state->omega-space s)
                       (up '(/ (+ (* 2 a b^) (* -2 a^ b) (*  2 c d^) (* -2 c^ d)) (+ (expt a 2) (expt b 2) (expt c 2) (expt d 2)))
                           '(/ (+ (* 2 a c^) (* -2 a^ c) (* -2 b d^) (*  2 b^ d)) (+ (expt a 2) (expt b 2) (expt c 2) (expt d 2)))
                           '(/ (+ (* 2 a d^) (* -2 a^ d) (*  2 b c^) (* -2 b^ c)) (+ (expt a 2) (expt b 2) (expt c 2) (expt d 2)))))
    (check-simplified? ((qw-state->L-body 'A 'B 'C) qw)
                       '(down (* A a^) (* B b^) (* C c^)))
    (check-simplified? ((qw-state->L-space 'A 'B 'C) qw)
                       '(down (/ (+ (* A (expt a 2) a^) (* A a^ (expt b 2)) (* -1 A a^ (expt c 2)) (* -1 A a^ (expt d 2)) (* -2 B a b^ d) (* 2 B b b^ c) (* 2 C a c c^) (* 2 C b c^ d)) (+ (expt a 2) (expt b 2) (expt c 2) (expt d 2)))
                              (/ (+ (* 2 A a a^ d) (* 2 A a^ b c) (* B (expt a 2) b^) (* -1 B (expt b 2) b^) (* B b^ (expt c 2)) (* -1 B b^ (expt d 2)) (* -2 C a b c^) (* 2 C c c^ d)) (+ (expt a 2) (expt b 2) (expt c 2) (expt d 2)))
                              (/ (+ (* -2 A a a^ c) (* 2 A a^ b d) (* 2 B a b b^) (* 2 B b^ c d) (* C (expt a 2) c^) (* -1 C (expt b 2) c^) (* -1 C (expt c 2) c^) (* C c^ (expt d 2))) (+ (expt a 2) (expt b 2) (expt c 2) (expt d 2)))))
    (check-simplified? ((T-quaternion-state 'A 'B 'C) s)
                       '(/ (+ (* 2 A (expt a 2) (expt b^ 2)) (* -4 A a a^ b b^) (* -4 A a b^ c d^) (* 4 A a b^ c^ d) (* 2 A (expt a^ 2) (expt b 2)) (* 4 A a^ b c d^) (* -4 A a^ b c^ d) (* 2 A (expt c 2) (expt d^ 2)) (* -4 A c c^ d d^)
                              (* 2 A (expt c^ 2) (expt d 2)) (* 2 B (expt a 2) (expt c^ 2)) (* -4 B a a^ c c^) (* 4 B a b c^ d^) (* -4 B a b^ c^ d) (* 2 B (expt a^ 2) (expt c 2)) (* -4 B a^ b c d^) (* 4 B a^ b^ c d) (* 2 B (expt b 2) (expt d^ 2))
                              (* -4 B b b^ d d^) (* 2 B (expt b^ 2) (expt d 2)) (* 2 C (expt a 2) (expt d^ 2)) (* -4 C a a^ d d^) (* -4 C a b c^ d^) (* 4 C a b^ c d^) (* 2 C (expt a^ 2) (expt d 2)) (* 4 C a^ b c^ d) (* -4 C a^ b^ c d)
                              (* 2 C (expt b 2) (expt c^ 2)) (* -4 C b b^ c c^) (* 2 C (expt b^ 2) (expt c 2)))
                           (+ (expt a 4) (* 2 (expt a 2) (expt b 2)) (* 2 (expt a 2) (expt c 2)) (* 2 (expt a 2) (expt d 2)) (expt b 4) (* 2 (expt b 2) (expt c 2)) (* 2 (expt b 2) (expt d 2)) (expt c 4) (* 2 (expt c 2) (expt d 2)) (expt d 4)))))

   ;;*************************************************************************************************
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
    (check-= (* (/ 60 n:2pi) (/ 7.734804457773965e-3 6.6e-5)) 1119.1203302763215 1e-10))
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