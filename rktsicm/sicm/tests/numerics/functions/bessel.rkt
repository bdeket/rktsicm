#lang racket/base

(require rackunit
         "../../../main.rkt"
         )

(provide the-tests)
(define the-tests
  (test-suite
   "numerics/functions/bessel"
   (test-case "bessel-check"
              (define (bessel-check n x)
                (/ (- (* (bessj (+ n 1) x) (bessy n x))
                      (* (bessj n x) (bessy (+ n 1) x))
                      (/ 2 (* pi x)))
                   (/ 2 (* pi x))))
              
              (check-within (let lp ((x .0001) (worstx 0.0) (relerr 0.0))
                   (if (> x 20)
                       (list worstx relerr)
                       (let ((err (bessel-check 0 x)))
                         (if (> (magnitude err) (magnitude relerr))
                             (lp (+ x .0001) x err)
                             (lp (+ x .0001) worstx relerr)))))
                 '(7.950999999994808 -1.1214144656680372e-13)
                 1e-12))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))