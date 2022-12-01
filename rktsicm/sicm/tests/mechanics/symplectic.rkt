#lang s-exp "../../main.rkt"

(require rackunit
         "../helper.rkt")

(rename-part 'derivative 'D)

(define the-tests
  (test-suite
   "mechanics/symplectic"
   (check-simplified? ((canonical-transform? (F->CT p->r))
                       (up 't
                           (up 'r 'phi)
                           (down 'p_r 'p_phi)))
                      '(up (up 0 (up 0 0) (down 0 0))
                           (up (up 0 (up 0 0) (down 0 0)) (up 0 (up 0 0) (down 0 0)))
                           (down (up 0 (up 0 0) (down 0 0)) (up 0 (up 0 0) (down 0 0))))
                      )
   (check-simplified? ((canonical-transform? (polar-canonical 'alpha))
                       (up 't 'a 'I))
                      '(up (up 0 0 0) (up 0 0 0) (up 0 0 0)))
   (test-case
    "3"
    (define (a-non-canonical-transform Istate)
      (let ((t (time Istate))
            (theta (coordinate Istate))
            (p (momentum Istate)))
        (let ((x (* p (sin theta)))
              (p_x (* p (cos theta))))
          (up t x p_x))))
    (check-simplified? ((canonical-transform? a-non-canonical-transform)
                        (up 't 'theta 'p))
                       '(up (up 0 0 0) (up 0 0 (+ -1 p)) (up 0 (+ 1 (* -1 p)) 0))))
   (test-case
    "4"
    (define (Cmix H-state)
      (let ((t (time H-state))
            (q (coordinate H-state))
            (p (momentum H-state)))
        (up t
            (up (ref q 0) (- (ref p 1)))
            (down (ref p 0) (ref q 1)))))
    (define a-state
      (up 't (up 'x 'y) (down 'p_x 'p_y)))
    (check-simplified? ((canonical-transform? Cmix) a-state)
                       '(up (up 0 (up 0 0) (down 0 0))
                            (up (up 0 (up 0 0) (down 0 0)) (up 0 (up 0 0) (down 0 0)))
                            (down (up 0 (up 0 0) (down 0 0)) (up 0 (up 0 0) (down 0 0)))))
    (define (Cmix2 H-state)
      (let ((t (time H-state))
            (q (coordinate H-state))
            (p (momentum H-state)))
        (up t
            (flip-outer-index p)
            (- (flip-outer-index q)))))
    (check-simplified? ((canonical-transform? Cmix2)
                        a-state)
                       '(up (up 0 (up 0 0) (down 0 0))
                            (up (up 0 (up 0 0) (down 0 0)) (up 0 (up 0 0) (down 0 0)))
                            (down (up 0 (up 0 0) (down 0 0)) (up 0 (up 0 0) (down 0 0))))))
   (test-case
    "5"
    (define ((C m0 m1) state)
      (let ((x (coordinate state))
            (p (momentum state)))
        (let ((x0 (ref x 0))
              (x1 (ref x 1))
              (p0 (ref p 0))
              (p1 (ref p 1)))
          (up (time state)
              (up (/ (+ (* m0 x0) (* m1 x1)) (+ m0 m1))
                  (- x1 x0))
              (down (+ p0 p1)
                    (/ (- (* m0 p1) (* m1 p0))
                       (+ m0 m1)))))))
    (define b-state
      (up 't
          (up (up 'x_1 'y_1)
              (up 'x_2 'y_2))
          (down (down 'p_x_1 'p_y_1)
                (down 'p_x_2 'p_y_2))))
    (check-simplified? ((canonical-transform? (C 'm1 'm2)) b-state)
                       '(up
                         (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))
                         (up
                          (up (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))
                              (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0))))
                          (up (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))
                              (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))))
                         (down
                          (down (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))
                                (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0))))
                          (down (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))
                                (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0))))))))
   (test-case
    "6"
    (check-simplified? ((symplectic? (F->CT p->r))
                        (up 't
                            (up 'r 'phi)
                            (down 'p_r 'p_phi)))
                       '(matrix-by-rows (list 0 0 0 0 0)
                                        (list 0 0 0 0 0)
                                        (list 0 0 0 0 0)
                                        (list 0 0 0 0 0)
                                        (list 0 0 0 0 0))))
   (test-case
    "7"
    (define (a-non-canonical-transform Istate)
      (let ((t (time Istate))
            (theta (coordinate Istate))
            (p (momentum Istate)))
        (let ((x (* p (sin theta)))
              (p_x (* p (cos theta))))
          (up t x p_x))))
    (check-simplified? ((symplectic? a-non-canonical-transform)
                        (up 't 'theta 'p))
                       '(matrix-by-rows (list 0 0 0)
                                        (list 0 0 (+ 1 (* -1 p)))
                                        (list 0 (+ -1 p) 0)))
    (check-simplified? ((symplectic-transform? a-non-canonical-transform)
                        (up 't 'theta 'p))
                       '(matrix-by-rows (list 0 (+ 1 (* -1 p))) (list (+ -1 p) 0))))
   (check-simplified? ((symplectic? (polar-canonical 'alpha))
                       (up 't 'a 'I))
                      '(matrix-by-rows (list 0 0 0)
                                       (list 0 0 0)
                                       (list 0 0 0)))
   (test-case
    "9"
    (define (Cmix H-state)
      (let ((t (time H-state))
            (q (coordinate H-state))
            (p (momentum H-state)))
        (up t
            (up (ref q 0) (- (ref p 1)))
            (down   (ref p 0) (ref q 1)))))
    (define a-state
      (up 't (up 'x 'y) (down 'p_x 'p_y)))
    (check-simplified? ((symplectic? Cmix) a-state)
                       '(matrix-by-rows (list 0 0 0 0 0)
                                        (list 0 0 0 0 0)
                                        (list 0 0 0 0 0)
                                        (list 0 0 0 0 0)
                                        (list 0 0 0 0 0))))
   (test-case
    "11"
    (check-simplified? ((symplectic-transform? (F->CT p->r))
                        (up 't
                            (up 'r 'theta)
                            (down 'p_r 'p_theta)))
                       '(matrix-by-rows (list 0 0 0 0)
                                        (list 0 0 0 0)
                                        (list 0 0 0 0)
                                        (list 0 0 0 0))))
   (test-case
    "12"
    (define ((polar-canonical alpha) Istate)
      (let ((t (time Istate))
            (theta (coordinate Istate))
            (I (momentum Istate)))
        (let ((x (* (sqrt (/ (* 2 I) alpha)) (sin theta)))
              (p_x (* (sqrt (* 2 alpha I)) (cos theta))))
          (up t x p_x))))
    (define ((polar-canonical-inverse alpha) s)
      (let ((t (time s))
            (x (coordinate s))
            (p (momentum s)))
        (let ((I (/ (+ (* alpha (square x))
                       (/ (square p) alpha))
                    2)))
          (let ((theta (atan (/ x (sqrt (/ (* 2 I) alpha)))
                             (/ p (sqrt (* 2 I alpha))))))
            (up t theta I)))))
    (check-simplified? ((compose (polar-canonical-inverse 'alpha)
                                 (polar-canonical 'alpha))
                        (up 't 'x 'p))
                       '(up t x p))
    (check-simplified? ((symplectic-transform? (polar-canonical 'alpha))
                        (up 't 'a 'I))
                       '(matrix-by-rows (list 0 0) (list 0 0))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))