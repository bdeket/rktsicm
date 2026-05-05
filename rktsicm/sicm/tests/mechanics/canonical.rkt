#lang s-exp "../../main.rkt"

(require rackunit
         "../helper+scm.rkt")

(rename-part 'derivative 'D)

(provide the-tests)
(define the-tests
  (test-suite
   "mechanics/canonical"
   ;; canonical? -> check if H and H* are each others canonical transforms over C
   ;; the output of these should restult in zero? like structures to be true
   (test-case
    "T-func"
    (check-equal? (T-func (up 't (up 'x 'y) (down 'q 'r)))
                  (up 1 (up 0 0) (down 0 0))))
   (test-case
    "canonical-H?"
    (check-simplified? ((canonical-H? (F->CT p->r) (literal-function 'H (Hamiltonian 2)))
                        (up 't
                            (coordinate-tuple 'r 'phi)
                            (momentum-tuple 'p_r 'p_phi)))
                       (up 0 (up 0 0) (down 0 0))))
   (test-case
    "canonical-K?" ;; Kamiltonian
    (check-simplified? ((canonical-K? (F->CT p->r) (literal-function 'K (Hamiltonian 2)))
                        (up 't
                            (coordinate-tuple 'r 'phi)
                            (momentum-tuple 'p_r 'p_phi)))
                       '(up 0
                            (up (+ (* r (((partial 2 1) K) (up t (up r phi) (down p_r p_phi))) (sin phi))
                                     (* -1 (((partial 2 0) K) (up t (up r phi) (down p_r p_phi))) (cos phi)))
                                  (+ (* -1 r (((partial 2 1) K) (up t (up r phi) (down p_r p_phi))) (cos phi))
                                     (* -1 (sin phi) (((partial 2 0) K) (up t (up r phi) (down p_r p_phi))))))
                            (down (/ (+ (* p_r (expt r 2) (((partial 2 1) K) (up t (up r phi) (down p_r p_phi))) (sin phi))
                                        (* p_phi r (((partial 2 1) K) (up t (up r phi) (down p_r p_phi))) (cos phi))
                                        (* (expt r 2) (cos phi) (((partial 1 0) K) (up t (up r phi) (down p_r p_phi))))
                                        (* -1 p_phi (sin phi) (((partial 2 0) K) (up t (up r phi) (down p_r p_phi))))
                                        (* -1 r (sin phi) (((partial 1 1) K) (up t (up r phi) (down p_r p_phi)))))
                                     (expt r 2))
                                  (/ (+ (* -1 p_r (expt r 2) (((partial 2 1) K) (up t (up r phi) (down p_r p_phi))) (cos phi))
                                        (* p_phi r (((partial 2 1) K) (up t (up r phi) (down p_r p_phi))) (sin phi))
                                        (* (expt r 2) (sin phi) (((partial 1 0) K) (up t (up r phi) (down p_r p_phi))))
                                        (* p_phi (((partial 2 0) K) (up t (up r phi) (down p_r p_phi))) (cos phi))
                                        (* r (cos phi) (((partial 1 1) K) (up t (up r phi) (down p_r p_phi)))))
                                     (expt r 2))))))
   ;**************************************************************************************************
   (test-case
    "1"
    (define ((H-central m V) state)
      (let ((x (coordinate state))
            (p (momentum state)))
        (+ (/ (square p) (* 2 m))
           (V (sqrt (square x))))))
    (check-simplified? ((compositional-canonical? (F->CT p->r)
                                                 (H-central 'm (literal-function 'V)))
                       (up 't
                           (coordinate-tuple 'r 'phi)
                           (momentum-tuple 'p_r 'p_phi)))
                      '(up 0 (up 0 0) (down 0 0))))
   (check-simplified? ((time-independent-canonical? (F->CT p->r))
                       (up 't
                           (coordinate-tuple 'r 'phi)
                           (momentum-tuple 'p_r 'p_phi)))
                      '(up 0 (up 0 0) (down 0 0)))
   (test-case
    "3"
    (define (a-non-canonical-transform Istate)
      (let ((t (time Istate))
            (theta (coordinate Istate))
            (p (momentum Istate)))
        (let ((x (* p (sin theta)))
              (p_x (* p (cos theta))))
          (up t x p_x))))
    (check-unique-match? (simplify ((time-independent-canonical? a-non-canonical-transform)
                                    (up 't 'theta 'p)))
                         (x1 x2)
                         `(up 0 (+ (* -1 p ,x2) ,x2) (+ (* p ,x1) (* -1 ,x1))))
    )
   (check-simplified? ((compose (polar-canonical-inverse 'alpha)
                                (polar-canonical 'alpha))
                       (up 't 'x 'p))
                      '(up t x p))
   (check-simplified? ((time-independent-canonical? (polar-canonical 'alpha))
                       (up 't 'a 'I))
                      '(up 0 0 0))
   (test-case
    "5"
    (define (Cmix H-state)
      (let ((t (time H-state))
            (q (coordinate H-state))
            (p (momentum H-state)))
        (up t
            (coordinate-tuple (ref q 0) (- (ref p 1)))
            (momentum-tuple   (ref p 0) (ref q 1)))))
    (define a-state
      (up 't 
          (coordinate-tuple 'x 'y)
          (momentum-tuple 'p_x 'p_y)))
    (check-simplified? ((time-independent-canonical? Cmix)
                        a-state)
                       '(up 0 (up 0 0) (down 0 0)))
    (define (Cmix2 H-state)
      (let ((t (time H-state))
            (q (coordinate H-state))
            (p (momentum H-state)))
        (up t
            (flip-outer-index p)
            (- (flip-outer-index q)))))
    (check-simplified? ((time-independent-canonical? Cmix2)
                        a-state)
                       '(up 0 (up 0 0) (down 0 0))))
   (test-case
    "6"
    (define b-state
      (up 't
          (coordinate-tuple
           (coordinate-tuple 'x_1 'y_1)
           (coordinate-tuple 'x_2 'y_2))
          (momentum-tuple
           (momentum-tuple 'p_x_1 'p_y_1)
           (momentum-tuple 'p_x_2 'p_y_2))))
    (check-simplified? (- ((F->CT (two-particle-center-of-mass 'm0 'm1)) b-state)
                          ((two-particle-center-of-mass-canonical 'm0 'm1) b-state))
                       '(up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0))))
    (check-simplified? ((time-independent-canonical?
                         (two-particle-center-of-mass-canonical 'm1 'm2))
                        b-state)
                       '(up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))))
   (test-case
    "7"
    (define (T v)
      (* (down (up 'a 'c) (up 'b 'd)) v))
    (check-simplified? (T (up 'x 'y))
                       '(up (+ (* a x) (* b y)) (+ (* c x) (* d y))))
    (check-simplified? (* (* (down 'p_x 'p_y) ((D T) (up 'x 'y))) (up 'v_x 'v_y))
                       '(+ (* a p_x v_x) (* b p_x v_y) (* c p_y v_x) (* d p_y v_y)))
    (check-simplified? (* (down 'p_x 'p_y) (* ((D T) (up 'x 'y)) (up 'v_x 'v_y)))
                       '(+ (* a p_x v_x) (* b p_x v_y) (* c p_y v_x) (* d p_y v_y)))
    (check-simplified? (* (* ((multiplicative-transpose (down 'p_x 'p_y)) ((D T) (up 'x 'y)))
                             (down 'p_x 'p_y))
                          (up 'v_x 'v_y))
                       '(+ (* a p_x v_x) (* b p_x v_y) (* c p_y v_x) (* d p_y v_y)))
    (check-simplified? (* (* (down 'p_x 'p_y)
                             ((multiplicative-transpose (down 'p_x 'p_y)) ((D T) (up 'x 'y))))
                          (up 'v_x 'v_y))
                       '(+ (* a p_x v_x) (* b p_x v_y) (* c p_y v_x) (* d p_y v_y))))
   (test-case
    "8"
    (check-simplified? ((time-independent-canonical? (F->CT p->r))
                        (up 't
                            (coordinate-tuple 'r 'phi)
                            (momentum-tuple 'p_r 'p_phi)))
                       '(up 0 (up 0 0) (down 0 0))))
   (test-case
    "10"
    (define ((C m0 m1) state)
      (let ((x (coordinate state))
            (p (momentum state)))
        (let ((x0 (ref x 0))
              (x1 (ref x 1))
              (p0 (ref p 0))
              (p1 (ref p 1)))
          (up 
           (time state)
           (coordinate-tuple (/ (+ (* m0 x0) (* m1 x1)) (+ m0 m1))
                             (- x1 x0))
           (momentum-tuple (+ p0 p1)
                           (/ (- (* m0 p1) (* m1 p0))
                              (+ m0 m1)))))))
    (define b-state
      (up 't
          (coordinate-tuple
           (coordinate-tuple 'x_1 'y_1)
           (coordinate-tuple 'x_2 'y_2))
          (momentum-tuple
           (momentum-tuple 'p_x_1 'p_y_1)
           (momentum-tuple 'p_x_2 'p_y_2))))
    (check-simplified? ((time-independent-canonical? (C 'm1 'm2)) b-state)
                       '(up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))