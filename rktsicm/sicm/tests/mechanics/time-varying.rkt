#lang s-exp "../../main.rkt"

(require rackunit
         "../helper.rkt")

(rename-part 'derivative 'D)

(define the-tests
  (test-suite
   "mechanics/time-varying"
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