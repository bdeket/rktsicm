#lang s-exp "../../main.rkt"

(require rackunit
         "../helper.rkt")

(rename-part 'derivative 'D)

(provide the-tests)
(define the-tests
  (test-suite
   "mechanics/Hamiltonian"
   (check-simplified? (matrix->H-state (H-state->matrix (up 't (up 'x 'y) (down 'p_x 'p_y)))
                                       ;;bdk;; missing argument...
                                       (up 't (up 'x 'y) (down 'px 'py)))
                      '(up t (up x y) (down p_x p_y)))
   (check-simplified? (H-state->matrix
                       (matrix->H-state
                        (matrix-by-rows (list 't)
                                        (list 'x)
                                        (list 'y)
                                        (list 'p_x)
                                        (list 'p_y))
                        ;;bdk;; missing argument...
                        (up 't (up 'x 'y) (down 'px 'py))))
                      '(matrix-by-rows (list t) (list x) (list y) (list p_x) (list p_y)))
   (test-case
    "H-rectangular"
    (define ((H-rectangular m V) H-state)
      (let ((q (coordinate H-state))
            (p (momentum H-state)))
        (+ (/ (square p) (* 2 m))
           (V (ref q 0) (ref q 1)))))
    (check-simplified? (((Hamilton-equations
                          (H-rectangular 
                           'm
                           (literal-function 'V (-> (X Real Real) Real))))
                         (coordinate-tuple (literal-function 'x)
                                           (literal-function 'y))
                         (momentum-tuple (literal-function 'p_x)
                                         (literal-function 'p_y)))
                        't)
                       '(up
                         0
                         (up (+ ((D x) t) (/ (* -1 (p_x t)) m))
                             (+ ((D y) t) (/ (* -1 (p_y t)) m)))
                         (down (+ ((D p_x) t) (((partial 0) V) (x t) (y t)))
                               (+ ((D p_y) t) (((partial 1) V) (x t) (y t)))))))
   (test-case
    "L-rectangular"
    (define ((L-rectangular m V) local)
      (let ((q (coordinate local))
            (qdot (velocity local)))
        (- (* 1/2 m (square qdot))
           (V (ref q 0) (ref q 1)))))
    (check-simplified? ((Lagrangian->Hamiltonian
                         (L-rectangular 'm
                                        (literal-function 'V
                                                          (-> (X Real Real) Real))))
                        (->H-state 't
                                   (coordinate-tuple 'x 'y)
                                   (momentum-tuple 'p_x 'p_y)))
                       '(+ (V x y)
                           (/ (* 1/2 (expt p_x 2)) m)
                           (/ (* 1/2 (expt p_y 2)) m))))
   (test-case
    "L-central-polar"
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
    (check-simplified? ((Lagrangian->Hamiltonian 
                         (L-central-polar 'm (literal-function 'V)))
                        (->H-state 't
                                   (coordinate-tuple 'r 'phi)
                                   (momentum-tuple 'p_r 'p_phi)))
                       '(+ (V r)
                           (/ (* 1/2 (expt p_r 2)) m)
                           (/ (* 1/2 (expt p_phi 2)) (* m (expt r 2)))))
    (check-simplified? (((Hamilton-equations
                          (Lagrangian->Hamiltonian 
                           (L-central-polar 'm (literal-function 'V))))
                         (coordinate-tuple (literal-function 'r)
                                           (literal-function 'phi))
                         (momentum-tuple (literal-function 'p_r)
                                         (literal-function 'p_phi)))
                        't)
                       '(up
                         0
                         (up (+ ((D r) t) (/ (* -1 (p_r t)) m))
                             (+ ((D phi) t) (/ (* -1 (p_phi t)) (* m (expt (r t) 2)))))
                         (down
                          (+ ((D p_r) t)
                             ((D V) (r t))
                             (/ (* -1 (expt (p_phi t) 2)) (* m (expt (r t) 3))))
                          ((D p_phi) t))))
    ;;; If we substitute a Coulomb potential in for V we get the equations 
    ;;;  for satellite motion around a spherical primary.
    (check-simplified? (((Hamilton-equations
                          (Lagrangian->Hamiltonian
                           (L-central-polar 'm
                                            (lambda (r)
                                              (- (/ (* 'GM 'm) r))))))
                         (coordinate-tuple (literal-function 'r)
                                           (literal-function 'phi))
                         (momentum-tuple (literal-function 'p_r)
                                         (literal-function 'p_phi)))
                        't)
                       '(up 0
                            (up (+ ((D r) t) (/ (* -1 (p_r t)) m))
                                (+ ((D phi) t) (/ (* -1 (p_phi t)) (* m (expt (r t) 2)))))
                            (down
                             (+ ((D p_r) t)
                                (/ (* GM m) (expt (r t) 2))
                                (/ (* -1 (expt (p_phi t) 2)) (* m (expt (r t) 3))))
                             ((D p_phi) t)))))
   (test-case
    "L-harmonic"
    (define ((L-harmonic m k) local)
      (let ((q (coordinate local)) 
            (v (velocity local)))
        (- (* 1/2 m (square v))
           (* 1/2 k (square q)))))
    (check-simplified? (((Hamilton-equations
                          (Lagrangian->Hamiltonian (L-harmonic 'm 'k)))
                         (coordinate-tuple (literal-function 'x_1)
                                           (literal-function 'x_2))
                         (momentum-tuple (literal-function 'p_1)
                                         (literal-function 'p_2)))
                        't)
                       '(up
                         0
                         (up
                          (/ (+ (* m ((D x_1) t)) (* -1 (p_1 t))) m)
                          (/ (+ (* m ((D x_2) t)) (* -1 (p_2 t))) m))
                         (down (+ (* k (x_1 t)) ((D p_1) t)) (+ (* k (x_2 t)) ((D p_2) t))))
                       #; ;;bdk;; c, m_1 ... seems wrong
                       '(up 0
                            (up (+ ((D x_1) t) (/ (* -1 (p_1 t)) m_1))
                                (+ ((D x_2) t) (/ (* -1 (p_2 t)) m_2)))
                            (down (+ (* c (x_2 t)) (* k_1 (x_1 t)) ((D p_1) t))
                                  (+ (* c (x_1 t)) (* k_2 (x_2 t)) ((D p_2) t))))))
   (test-case
    "L-couplled-harmonic"
    (define ((L-coupled-harmonic m k) state)
      (let ((q (coordinate state))
            (qdot (velocity state)))
        (- (* 1/2 qdot m qdot)
           (* 1/2 q k q))))
    (check-simplified? ((Lagrangian->Hamiltonian
                         (L-coupled-harmonic (down (down 'm_1 0)
                                                   (down 0 'm_2))
                                             (down (down 'k_1 'c)
                                                   (down 'c 'k_2))))
                        (->H-state 't
                                   (coordinate-tuple 'x_1 'x_2)
                                   (momentum-tuple 'p_1 'p_2)))
                       '(+ (* c x_1 x_2)
                           (* 1/2 k_1 (expt x_1 2))
                           (* 1/2 k_2 (expt x_2 2))
                           (/ (* 1/2 (expt p_2 2)) m_2)
                           (/ (* 1/2 (expt p_1 2)) m_1)))
    (check-simplified? (((Hamilton-equations
                          (Lagrangian->Hamiltonian
                           (L-coupled-harmonic (down (down 'm_1 0)
                                                     (down 0 'm_2))
                                               (down (down 'k_1 'c)
                                                     (down 'c 'k_2)))))
                         (coordinate-tuple (literal-function 'x_1)
                                           (literal-function 'x_2))
                         (momentum-tuple (literal-function 'p_1)
                                         (literal-function 'p_2)))
                        't)
                       '(up
                         0
                         (up (+ ((D x_1) t) (/ (* -1 (p_1 t)) m_1))
                             (+ ((D x_2) t) (/ (* -1 (p_2 t)) m_2)))
                         (down (+ (* c (x_2 t)) (* k_1 (x_1 t)) ((D p_1) t))
                               (+ (* c (x_1 t)) (* k_2 (x_2 t)) ((D p_2) t))))))
   (test-case
    "L-two-particle"
    (define ((L-two-particle m1 m2) local)
      (let ((x (coordinate local))
            (v (velocity local))
            (V (literal-function 'V (-> (X (^ Real 2) (^ Real 2)) Real))))
        (let ((x1 (ref x 0)) (x2 (ref x 1))
                             (v1 (ref v 0)) (v2 (ref v 1)))
          (- (+ (* 1/2 m1 (square v1))
                (* 1/2 m2 (square v2)))
             (V x1 x2)))))
    (check-simplified? (((Hamilton-equations
                          (Lagrangian->Hamiltonian
                           (L-two-particle 'm_1 'm_2)))
                         (coordinate-tuple (coordinate-tuple (literal-function 'x_1)
                                                             (literal-function 'y_1))
                                           (coordinate-tuple (literal-function 'x_2)
                                                             (literal-function 'y_2)))
                         (momentum-tuple (momentum-tuple (literal-function 'p_x_1)
                                                         (literal-function 'p_y_1))
                                         (momentum-tuple (literal-function 'p_x_2)
                                                         (literal-function 'p_y_2))))
                        't)
                       '(up 0
                            (up (up (+ ((D x_1) t) (/ (* -1 (p_x_1 t)) m_1))
                                    (+ ((D y_1) t) (/ (* -1 (p_y_1 t)) m_1)))
                                (up (+ ((D x_2) t) (/ (* -1 (p_x_2 t)) m_2))
                                    (+ ((D y_2) t) (/ (* -1 (p_y_2 t)) m_2))))
                            (down (down
                                   (+ (((partial 0 0) V)
                                       (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t)))
                                      ((D p_x_1) t))
                                   (+ (((partial 0 1) V)
                                       (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t)))
                                      ((D p_y_1) t)))
                                  (down
                                   (+ (((partial 1 0) V)
                                       (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t)))
                                      ((D p_x_2) t))
                                   (+ (((partial 1 1) V)
                                       (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t)))
                                      ((D p_y_2) t)))))))
   (test-case
    "Phase-space reduction"
    (define ((L-axisymmetric-top A C gMR) local)
      (let ((q (coordinate local))
            (qdot (velocity local)))
        (let ((theta (ref q 0))
              (thetadot (ref qdot 0))
              (phidot (ref qdot 1))
              (psidot (ref qdot 2)))
          (+ (* 1/2 A
                (+ (square thetadot)
                   (square (* phidot (sin theta)))))
             (* 1/2 C
                (square (+ psidot (* phidot (cos theta)))))
             (* -1 gMR (cos theta))))))
    (check-simplified? ((Lagrangian->Hamiltonian (L-axisymmetric-top 'A 'C 'gMR)) 
                        (->H-state 't
                                   (vector 'theta 'phi 'psi)
                                   (vector 'p_theta 'p_phi 'p_psi)))
                       '(+ (* gMR (cos theta))
                           (/ (* 1/2 (expt p_psi 2)) C)
                           (/ (* 1/2 (expt p_psi 2) (expt (cos theta) 2)) (* A (expt (sin theta) 2)))
                           (/ (* 1/2 (expt p_theta 2)) A)
                           (/ (* -1 p_phi p_psi (cos theta)) (* A (expt (sin theta) 2)))
                           (/ (* 1/2 (expt p_phi 2)) (* A (expt (sin theta) 2)))))
    (check-simplified? (((Hamilton-equations
                         (Lagrangian->Hamiltonian
                          (L-axisymmetric-top 'A 'C 'gMR)))
                        (coordinate-tuple (literal-function 'theta)
                                          (literal-function 'phi)
                                          (literal-function 'psi))
                        (momentum-tuple (literal-function 'p_theta)
                                        (literal-function 'p_phi)
                                        (literal-function 'p_psi)))
                       't)
                      '(up
                        0
                        (up
                         (+ ((D theta) t) (/ (* -1 (p_theta t)) A))
                         (+ ((D phi) t)
                            (/ (* (cos (theta t)) (p_psi t)) (* A (expt (sin (theta t)) 2)))
                            (/ (* -1 (p_phi t)) (* A (expt (sin (theta t)) 2))))
                         (+
                          ((D psi) t)
                          (/ (* -1 (p_psi t)) C)
                          (/ (* -1 (expt (cos (theta t)) 2) (p_psi t)) (* A (expt (sin (theta t)) 2)))
                          (/ (* (p_phi t) (cos (theta t))) (* A (expt (sin (theta t)) 2)))))
                        (down
                         (+
                          (/ (* -1 gMR (expt (cos (theta t)) 4)) (expt (sin (theta t)) 3))
                          ((D p_theta) t)
                          (/ (* 2 gMR (expt (cos (theta t)) 2)) (expt (sin (theta t)) 3))
                          (/ (* (p_phi t) (expt (cos (theta t)) 2) (p_psi t))
                             (* A (expt (sin (theta t)) 3)))
                          (/ (* -1 (cos (theta t)) (expt (p_psi t) 2)) (* A (expt (sin (theta t)) 3)))
                          (/ (* -1 (expt (p_phi t) 2) (cos (theta t))) (* A (expt (sin (theta t)) 3)))
                          (/ (* -1 gMR) (expt (sin (theta t)) 3))
                          (/ (* (p_phi t) (p_psi t)) (* A (expt (sin (theta t)) 3))))
                         ((D p_phi) t)
                         ((D p_psi) t)))))
   (test-case
    "poisson-bracket 1"
    (define a-state  (->H-state 't  (coordinate-tuple 'x 'y 'z)
                                (momentum-tuple 'p_x 'p_y 'p_z)))
    (check-simplified? ((Poisson-bracket
                         (up (compose (component 0) coordinate)
                             (compose (component 1) coordinate)
                             (compose (component 2) coordinate))
                         (down (compose (component 0) momentum)
                               (compose (component 1) momentum)
                               (compose (component 2) momentum)))
                        a-state)
                       '(up (down 1 0 0) (down 0 1 0) (down 0 0 1))))
   (test-case
    "poisson-bracket 2"
    (define FF
      (literal-function 'F
                        (-> (UP Real
                                (UP Real Real)
                                (DOWN Real Real))
                            Real)))
    (define GG
      (literal-function 'G
                        (-> (UP Real
                                (UP Real Real)
                                (DOWN Real Real))
                            Real)))
    (check-simplified? ((* (D FF)
                           (Poisson-bracket identity identity)
                           (D GG))
                        (up 't (up 'x 'y) (down 'px 'py)))
                       '(+ (* -1
                              (((partial 1 0) G) (up t (up x y) (down px py)))
                              (((partial 2 0) F) (up t (up x y) (down px py))))
                           (* -1
                              (((partial 1 1) G) (up t (up x y) (down px py)))
                              (((partial 2 1) F) (up t (up x y) (down px py))))
                           (* (((partial 2 0) G) (up t (up x y) (down px py)))
                              (((partial 1 0) F) (up t (up x y) (down px py))))
                           (* (((partial 2 1) G) (up t (up x y) (down px py)))
                              (((partial 1 1) F) (up t (up x y) (down px py)))))))
   (test-case
    "Jacobi identity"
    (define F (literal-function 'F (Hamiltonian 2)))
    (define G (literal-function 'G (Hamiltonian 2)))
    (define H (literal-function 'H (Hamiltonian 2)))
    (check-simplified? ((+ (Poisson-bracket F (Poisson-bracket G H))
                           (Poisson-bracket G (Poisson-bracket H F))
                           (Poisson-bracket H (Poisson-bracket F G)))
                        (up 't (up 'x 'y) (down 'px 'py)))
                       0))
   (test-case
    "poisson-bracket 3"
    (define Sx (compose (component 0) coordinate))
    (define Sy (compose (component 1) coordinate))
    (define Sz (compose (component 2) coordinate))
    (define Spx (compose (component 0) momentum))
    (define Spy (compose (component 1) momentum))
    (define Spz (compose (component 2) momentum))
    (define Lx (- (* Sy Spz) (* Spy Sz)))
    (define Ly (- (* Sz Spx) (* Spz Sx)))
    (define Lz (- (* Sx Spy) (* Spx Sy)))
    (define L (down Lx Ly Lz))
    (define 3-state 
      (->H-state 't 
                 (coordinate-tuple 'x 'y 'z)
                 (momentum-tuple 'p_x 'p_y 'p_z)))
    (check-simplified? ((Poisson-bracket Lx L) 3-state)
                       '(down 0 (+ (* -1 p_x y) (* p_y x)) (+ (* -1 p_x z) (* p_z x))))
    (define ((rotating n) state)
      (let ((t (time state))
            (q (coordinate state)))
        (let ((x (ref q 0))
              (y (ref q 1))
              (z (ref q 2)))
          (coordinate-tuple (+ (* (cos (* n t)) x) (* (sin (* n t)) y))
                            (- (* (cos (* n t)) y) (* (sin (* n t)) x))
                            z))))
    (define (C-rotating n) (F->CT (rotating n)))
    (check-simplified? ((- (compose (Poisson-bracket Lx Ly) (C-rotating 'n))
                           (Poisson-bracket (compose Lx (C-rotating 'n))
                                            (compose Ly (C-rotating 'n))) )
                        3-state)
                       0)
    ;;; Poisson brackets in terms of J 
    ;;;  Guaranteed to work only for scalar valued functions
    (define (J-func DH)
      (->H-state 0
                 (ref DH 2)
                 (- (ref DH 1))))
    (define ((PB f g) s)
      (* ((D f) s) (J-func ((D g) s))))
    (define a-state 
      (->H-state 't 
                 (coordinate-tuple 'x 'y 'z)
                 (momentum-tuple 'p_x 'p_y 'p_z)))
    (check-simplified? ((- (Poisson-bracket Lx Ly) Lz) a-state) 0)
    (check-simplified? ((- (PB Lx Ly) Lz) a-state) 0)
    (let ()
      (define ((L-harmonic m k) local)
        (let ((q (coordinate local)) 
              (v (velocity local)))
          (- (* 1/2 m (square v))
             (* 1/2 k (square q)))))
      (define ((PB f g) s)
        (let ((J (linear-function->multiplier J-func ((D g) s))))
          (* ((D f) s) (* J ((D g) s)))))
      (define (H-harmonic m k)
        (Lagrangian->Hamiltonian (L-harmonic m k)))
      (check-simplified? (- ((Poisson-bracket (H-harmonic 'm 'k)
                                              ((component 0) coordinate)) 
                             a-state)
                            ((PB (H-harmonic 'm 'k)
                                 (compose (component 0) coordinate))
                             a-state))
                         0)
      (check-simplified? (- ((Poisson-bracket (H-harmonic 'm 'k) coordinate) 
                             a-state)
                            ((PB (H-harmonic 'm 'k) coordinate)
                             a-state))
                         '(up 0 0 0))
      (check-simplified? ((PB momentum (H-harmonic 'm 'k))
                          a-state)
                         '(down (* -1 k x) (* -1 k y) (* -1 k z)))
      (check-simplified? ((PB coordinate (H-harmonic 'm 'k))
                          a-state)
                         '(up (/ p_x m) (/ p_y m) (/ p_z m)))
      ))
   (test-case
    "Lie derivatives"
    (define F (literal-function 'F (Hamiltonian 2)))
    (define G (literal-function 'G (Hamiltonian 2)))
    (define H (literal-function 'H (Hamiltonian 2)))
    (define L_F (Lie-derivative F))
    (define L_G (Lie-derivative G))
    (check-simplified? (((+ (commutator L_F L_G)
                            (Lie-derivative (Poisson-bracket F G)))
                         H)
                        (up 't (up 'x 'y) (down 'px 'py)))
                       0))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))