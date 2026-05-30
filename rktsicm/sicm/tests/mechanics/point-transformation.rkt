#lang s-exp "../../main.rkt"

(require rackunit
         "../../mechanics/point-transformation.rkt"
         "../helper+scm.rkt")

(rename-part 'derivative 'D)

(provide the-tests)
(define the-tests
  (test-suite
   "mechanics/point-transformation"
   (test-case
    "F->CT"
    (define ((H-central m V) state)
      (let ((x (coordinate state))
            (p (momentum state)))
        (+ (/ (square p) (* 2 m))
           (V (sqrt (square x))))))
    (check-simplified? ((compose (H-central 'm (literal-function 'V))
                                 (F->CT p->r))
                        (->H-state 't
                                   (coordinate-tuple 'r 'phi)
                                   (momentum-tuple 'p_r 'p_phi)))
                       '(+ (V r)
                           (/ (* 1/2 (expt p_r 2)) m)
                           (/ (* 1/2 (expt p_phi 2)) (* m (expt r 2))))))
   (test-case
    "F->K"
    (check-simplified? ((F->K p->r)
                        (->H-state 't (coordinate-tuple 'r 'phi) (momentum-tuple 'p_r 'p_phi)))
                       0)
    ;; same test as time-varying
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
    (check-simplified? ((canonical-K? (C-rotating 'n) (F->K (rotating 'n))) a-state)
                       '(up 0 (up 0 0 0) (down 0 0 0)))
    (check-simplified? ((- (F->K (rotating 'n)) (K 'n)) a-state)
                       0))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))