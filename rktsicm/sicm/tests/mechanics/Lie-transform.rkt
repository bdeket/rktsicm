#lang s-exp "../../main.rkt"

(require rackunit
         "../helper.rkt")

(rename-part 'derivative 'D)

(define the-tests
  (test-suite
   "mechanics/Lie-transform"
   (test-case
    "1"
    (define ((H-harmonic m k) state)
      (let ((q (coordinate state))
            (p (momentum state)))
        (+ (/ (square p) (* 2 m))
           (* 1/2 k (square q)))))
    (check-simplified? (accumulate print-expression
                                     (series:for-each print-expression
                                                      (((Lie-transform (H-harmonic 'm 'k) 'dt)
                                                        state->q)
                                                       (->H-state 0 'x_0 'p_0))
                                                      6))
                       '(x_0
                         (/ (* dt p_0) m)
                         (/ (* -1/2 (expt dt 2) k x_0) m)
                         (/ (* -1/6 (expt dt 3) k p_0) (expt m 2))
                         (/ (* 1/24 (expt dt 4) (expt k 2) x_0) (expt m 2))
                         (/ (* 1/120 (expt dt 5) (expt k 2) p_0) (expt m 3))))
    (check-simplified? (accumulate print-expression
                                   (series:for-each print-expression
                                                    (((Lie-transform (H-harmonic 'm 'k) 'dt)
                                                      momentum)
                                                     (->H-state 0 'x_0 'p_0))
                                                    6))
                       '(p_0
                         (* -1 dt k x_0)
                         (/ (* -1/2 (expt dt 2) k p_0) m)
                         (/ (* 1/6 (expt dt 3) (expt k 2) x_0) m)
                         (/ (* 1/24 (expt dt 4) (expt k 2) p_0) (expt m 2))
                         (/ (* -1/120 (expt dt 5) (expt k 3) x_0) (expt m 2))))
    (check-simplified? (accumulate print-expression
                                   (series:for-each print-expression
                                                    (((Lie-transform (H-harmonic 'm 'k) 'dt)
                                                      (H-harmonic 'm 'k))
                                                     (->H-state 0 'x_0 'p_0))
                                                    6))
                       '((/ (+ (* 1/2 k m (expt x_0 2)) (* 1/2 (expt p_0 2))) m)
                         0
                         0
                         0
                         0
                         0
                         )))
   (test-case
    "2"
    (define ((H-central-polar m V) state)
      (let ((q (coordinate state))
            (p (momentum state)))
        (let ((r ((component 0) q))
              (phi ((component 1) q))
              (pr ((component 0) p))
              (pphi ((component 1) p)))
          (+ (/ (+ (square pr)
                   (square (/ pphi r)))
                (* 2 m))
             (V r)))))
    (check-simplified? (accumulate print-expression
                                   (series:for-each print-expression
                                                    (((Lie-transform
                                                       (H-central-polar 'm (literal-function 'U))
                                                       'dt)
                                                      state->q)
                                                     (->H-state 0
                                                                (coordinate-tuple 'r_0 'phi_0)
                                                                (momentum-tuple 'p_r_0 'p_phi_0)))
                                                    4))
                       '((up r_0 phi_0)
                         (up (/ (* dt p_r_0) m) (/ (* dt p_phi_0) (* m (expt r_0 2))))
                         (up
                          (+ (/ (* -1/2 (expt dt 2) ((D U) r_0)) m)
                             (/ (* 1/2 (expt dt 2) (expt p_phi_0 2)) (* (expt m 2) (expt r_0 3))))
                          (/ (* -1 (expt dt 2) p_phi_0 p_r_0) (* (expt m 2) (expt r_0 3))))
                         (up
                          (+
                           (/ (* -1/6 (expt dt 3) p_r_0 (((expt D 2) U) r_0)) (expt m 2))
                           (/ (* -1/2 (expt dt 3) (expt p_phi_0 2) p_r_0) (* (expt m 3) (expt r_0 4))))
                          (+ (/ (* 1/3 (expt dt 3) p_phi_0 ((D U) r_0)) (* (expt m 2) (expt r_0 3)))
                             (/ (* -1/3 (expt dt 3) (expt p_phi_0 3)) (* (expt m 3) (expt r_0 6)))
                             (/ (* (expt dt 3) p_phi_0 (expt p_r_0 2)) (* (expt m 3) (expt r_0 4))))))))
   (test-case
    "3"
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
    (check-simplified? (accumulate pp
                                   (series:for-each pp
                                                    (((Lie-transform
                                                       (Lagrangian->Hamiltonian
                                                        (L-central-polar 'm (lambda (r) (- (/ 'GM r)))))
                                                       'dt)
                                                      state->q)
                                                     (->H-state 0
                                                                (coordinate-tuple 'r_0 'phi_0)
                                                                (momentum-tuple 'p_r_0 'p_phi_0)))
                                                    4))
                       '((up r_0 phi_0)
                         (up (/ (* dt p_r_0) m) (/ (* dt p_phi_0) (* m (expt r_0 2))))
                         (up
                          (+ (/ (* -1/2 GM (expt dt 2)) (* m (expt r_0 2)))
                             (/ (* 1/2 (* (expt dt 2) (expt p_phi_0 2))) (* (expt m 2) (expt r_0 3))))
                          (/ (* -1 (expt dt 2) p_r_0 p_phi_0) (* (expt m 2) (expt r_0 3))))
                         (up
                          (+ (/ (* 1/3 (* GM (expt dt 3) p_r_0)) (* (expt m 2) (expt r_0 3)))
                             (/ (* -1/2 (expt dt 3) p_r_0 (expt p_phi_0 2)) (* (expt m 3) (expt r_0 4))))
                          (+ (/ (* (expt dt 3) p_phi_0 (expt p_r_0 2)) (* (expt m 3) (expt r_0 4)))
                             (/ (* 1/3 (* GM (expt dt 3) p_phi_0)) (* (expt m 2) (expt r_0 5)))
                             (/ (* -1/3 (expt dt 3) (expt p_phi_0 3)) (* (expt m 3) (expt r_0 6))))))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))