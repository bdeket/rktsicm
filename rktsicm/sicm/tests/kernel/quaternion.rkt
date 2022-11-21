#lang racket/base

(require rackunit
         "../../main.rkt"
         "../../rkt/int.rkt"
         )

(define kernel-tests
  (test-suite
   "kernel/quaternion"
   (test-case "ORIG:quaternion->angle-axis >>> failing"
              (check-equal?
               (simplify
                (quaternion->angle-axis
                 (angle-axis->quaternion 'theta
                                         (up 'x 'y (sqrt (- 1 (square 'x) (square 'y)))))))
               '(theta (up x y (sqrt (+ 1 (* -1 (expt x 2)) (* -1 (expt y 2)))))))
              )
   
   ;TODO: this test currently fails, but the original notes mention
   ;some assumptions that are made (shownotes)
   ;maybe this example is incomplete?
   (test-case "ORIG:rotation-matrix->quaternion"
              (check-equal?
               (simplify
                (let* ([theta 'theta]
                       [v (up 'x 'y 'z)]
                       [axis (v:make-unit v)]
                       [result
                        ((compose quaternion->angle-axis
                                  rotation-matrix->quaternion
                                  quaternion->rotation-matrix
                                  angle-axis->quaternion)
                         theta axis)])
                  (up (- (car result) theta)
                      (- (cadr result) axis))))
               '(up 0 (up 0 0 0)))
              )

   (test-case "ORIG:rotation-matrix->quaternion"
              (check-equal?
               (simplify
                (let* ([theta -1]
                       [v (up 1 2 3)]
                       [axis (v:make-unit v)]
                       [result
                        ((compose quaternion->angle-axis
                                  rotation-matrix->quaternion
                                  quaternion->rotation-matrix
                                  angle-axis->quaternion)
                         theta axis)])
                  (up (- (car result) theta)
                      (- (cadr result) axis))))
               '(up 2.
                    (up -.5345224838248488
                        -1.0690449676496976
                        -1.6035674514745464)))
              )


   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests kernel-tests))