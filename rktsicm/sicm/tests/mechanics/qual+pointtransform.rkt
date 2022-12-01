#lang s-exp "../../main.rkt"

(require rackunit
         "../helper.rkt")

(rename-part 'derivative 'D)

(define the-tests
  (test-suite
   "mechanics/qualitative+point-transformationns"
   (test-case
    "qualitative"
    (check-= (find-invariant-curve (standard-map 0.95)
                      (- 1 (/ 1 (/ (+ 1 (sqrt 5)) 2)))
                      0.0
                      2.0
                      2.2
                      1e-5)
             2.114462280273437 1e12))
   (test-case
    "point-transformations"
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
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))