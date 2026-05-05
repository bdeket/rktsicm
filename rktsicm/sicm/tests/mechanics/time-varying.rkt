#lang s-exp "../../main.rkt"

(require rackunit
         "../helper+scm.rkt")

(rename-part 'derivative 'D)

(provide the-tests)
(define the-tests
  (test-suite
   "mechanics/time-varying"
   (test-case
    "F->K"
    (check-simplified? ((F->K (literal-function 'F (Hamiltonian 1))) (->H-state 0 'q0 'p0))
                       '(/ (* -1 p0 (((partial 0) F) (up 0 q0 p0))) (((partial 1) F) (up 0 q0 p0)))))
   (test-case
    "qp-canonical?"
    (check-simplified? ((qp-canonical? (literal-function 'C (-> (UP Real Real Real) (UP Real Real Real))) (literal-function 'H (Hamiltonian 1))) (->H-state 0 'q 'p))
                       '(up (+ (* (((partial 1) H) (up (C^0 (up 0 q p)) (C^1 (up 0 q p)) (C^2 (up 0 q p)))) (((partial 2) C^0) (up 0 q p)) (((partial 1) C^1) (up 0 q p)))
                               (* -1 (((partial 1) H) (up (C^0 (up 0 q p)) (C^1 (up 0 q p)) (C^2 (up 0 q p)))) (((partial 1) C^0) (up 0 q p)) (((partial 2) C^1) (up 0 q p)))
                               (* (((partial 2) C^0) (up 0 q p)) (((partial 2) H) (up (C^0 (up 0 q p)) (C^1 (up 0 q p)) (C^2 (up 0 q p)))) (((partial 1) C^2) (up 0 q p)))
                               (* -1 (((partial 1) C^0) (up 0 q p)) (((partial 2) H) (up (C^0 (up 0 q p)) (C^1 (up 0 q p)) (C^2 (up 0 q p)))) (((partial 2) C^2) (up 0 q p))))
                            (+ (* -1 (((partial 2) C^0) (up 0 q p)) (((partial 0) H) (up (C^0 (up 0 q p)) (C^1 (up 0 q p)) (C^2 (up 0 q p)))) (((partial 1) C^1) (up 0 q p)))
                               (* (((partial 0) H) (up (C^0 (up 0 q p)) (C^1 (up 0 q p)) (C^2 (up 0 q p)))) (((partial 1) C^0) (up 0 q p)) (((partial 2) C^1) (up 0 q p)))
                               (* (((partial 2) H) (up (C^0 (up 0 q p)) (C^1 (up 0 q p)) (C^2 (up 0 q p)))) (((partial 2) C^1) (up 0 q p)) (((partial 1) C^2) (up 0 q p)))
                               (* -1 (((partial 2) H) (up (C^0 (up 0 q p)) (C^1 (up 0 q p)) (C^2 (up 0 q p)))) (((partial 1) C^1) (up 0 q p)) (((partial 2) C^2) (up 0 q p)))
                               (((partial 2) H) (up (C^0 (up 0 q p)) (C^1 (up 0 q p)) (C^2 (up 0 q p)))))
                            (+ (* -1 (((partial 1) H) (up (C^0 (up 0 q p)) (C^1 (up 0 q p)) (C^2 (up 0 q p)))) (((partial 2) C^1) (up 0 q p)) (((partial 1) C^2) (up 0 q p)))
                               (* (((partial 1) H) (up (C^0 (up 0 q p)) (C^1 (up 0 q p)) (C^2 (up 0 q p)))) (((partial 1) C^1) (up 0 q p)) (((partial 2) C^2) (up 0 q p)))
                               (* -1 (((partial 2) C^0) (up 0 q p)) (((partial 0) H) (up (C^0 (up 0 q p)) (C^1 (up 0 q p)) (C^2 (up 0 q p)))) (((partial 1) C^2) (up 0 q p)))
                               (* (((partial 0) H) (up (C^0 (up 0 q p)) (C^1 (up 0 q p)) (C^2 (up 0 q p)))) (((partial 1) C^0) (up 0 q p)) (((partial 2) C^2) (up 0 q p)))
                               (* -1 (((partial 1) H) (up (C^0 (up 0 q p)) (C^1 (up 0 q p)) (C^2 (up 0 q p)))))))))

   ;**************************************************************************************************
   (test-case
    "1"
    (define ((K n) s)
      (let ((q (coordinate s))
            (p (momentum s)))
        (let ((x (ref q 0)) (y (ref q 1))
                            (px (ref p 0)) (py (ref p 1)))
          (* n (- (* x py) (* y px))))))
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
    (define a-state 
      (up 't 
          (coordinate-tuple 'x 'y 'z)
          (momentum-tuple 'p_x 'p_y 'p_z)))
    (check-simplified? ((canonical-K? (C-rotating 'n) (K 'n)) a-state)
                       '(up 0 (up 0 0 0) (down 0 0 0)))
    (check-simplified? ((canonical-K? (C-rotating 'n) (F->K (rotating 'n))) a-state)
                       '(up 0 (up 0 0 0) (down 0 0 0)))
    (check-simplified? ((- (F->K (rotating 'n))
                           (K 'n))
                        a-state)
                       0)
    (define ((bad-K n) s)
      (- ((K n) s)))
    (check-simplified? ((canonical-K? (C-rotating 'n) (bad-K 'n)) a-state)
                       '(up
                         0
                         (up (+ (* 2 n x (sin (* n t))) (* -2 n y (cos (* n t))))
                             (+ (* 2 n x (cos (* n t))) (* 2 n y (sin (* n t))))
                             0)
                         (down (+ (* 2 n p_x (sin (* n t))) (* -2 n p_y (cos (* n t))))
                               (+ (* 2 n p_x (cos (* n t))) (* 2 n p_y (sin (* n t))))
                               0))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))