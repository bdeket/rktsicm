#lang s-exp "../../main.rkt"

(require rackunit
         "../helper.rkt")

(rename-part 'derivative 'D)

(define the-tests
  (test-suite
   "mechanics/Noether"
   (test-case
    "1"
    (define ((L-central-rectangular m V) local)
      (let ((q (coordinate local))
            (v (velocity local)))
        (- (* 1/2 m (square v))
           (V (sqrt (square q))))))
    (define (F-tilde theta phi psi)
      (compose (Rx theta)
               (Ry phi)
               (Rz psi)
               coordinate))
    (check-simplified? ((Noether-integral
                         (L-central-rectangular 'm (literal-function 'Vr))
                         F-tilde)
                        (up 't
                            (up 'x 'y 'z)
                            (up 'vx 'vy 'vz)))
                       '(down (+ (* -1 m vy z) (* m vz y))
                              (+ (* m vx z) (* -1 m vz x))
                              (+ (* -1 m vx y) (* m vy x)))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))