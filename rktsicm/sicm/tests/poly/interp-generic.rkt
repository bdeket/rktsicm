#lang s-exp "../../main.rkt"

(require rackunit
         "../helper.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "poly/interp-generic"
   ;;; If run in generic environment we can look at the kind of thing that 
   ;;; this code does, by partial evaluation... an excellent aid to debugging. 
   (check-simplified? ((Lagrange-interpolation-function '(y1 y2 y3 y4) '(x1 x2 x3 x4)) 'x1)
                      'y1)
   (check-simplified? ((Lagrange-interpolation-function '(y1 y2 y3 y4) '(x1 x2 x3 x4)) 'x2)
                      'y2)
   (check-simplified? ((Lagrange-interpolation-function '(y1 y2 y3 y4) '(x1 x2 x3 x4)) 'x3)
                      'y3)
   (check-simplified? ((Lagrange-interpolation-function '(y1 y2 y3 y4) '(x1 x2 x3 x4)) 'x4)
                      'y4)
   ;; TODO : something goes wrong (sometimes) when running this wich check-simplified? => threading problem?
   (check-equal? (simplify ((Lagrange-interpolation-function '(y1 y2 y3 y4) '(x1 x2 x3 x4))
                            'x))
                 (simplify (let ((V-609 (- 'x3 'x4)) (V-607 (- 'x2 'x3)) (V-606 (* (- 'x2 'x4) -1))
                                                     (V-605 (- 'x 'x1))  (V-604 (- 'x1 'x4)) (V-603 (- 'x1 'x3))
                                                     (V-602 (- 'x1 'x2)) (V-601 (- 'x 'x2))  (V-599 (- 'x 'x3))
                                                     (V-598 (- 'x 'x4)))
                             (let ((V-608 (* V-601 V-605)) (V-600 (* V-598 V-599)))
                               (+ (/ (* V-600 V-601 'y1) (* V-602 V-603 V-604))
                                  (/ (* V-600 V-605 'y2) (* V-606 V-607 V-602))
                                  (/ (* V-608 V-598 'y3) (* V-603 V-607 V-609))
                                  (/ (* V-608 V-599 'y4) (* V-606 V-609 V-604)))))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))