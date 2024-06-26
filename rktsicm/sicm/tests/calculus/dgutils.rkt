#lang racket/base

(require rackunit
         "../../main.rkt"
         "../helper.rkt"
         )

(provide the-tests)
(define the-tests
  (test-suite
   "calculus/dgutils"
   (test-case
    "R2 rectangular"
    (define R2 R2-rect)
    (install-coordinates R2 (up 'x 'y))
    (define chi-R2 (R2 '->coords))
    (define chi-inverse-R2 (R2 '->point))
    (define R2-basis (coordinate-system->basis R2))
    (check-simplified? (s:sigma/r (lambda (e) 
                                    ((e (compose (literal-function 'f (-> (UP Real Real) Real))
                                                 chi-R2))
                                     (chi-inverse-R2 (up 'x0 'y0))))
                                  (basis->vector-basis R2-basis))
                       '(+ (((partial 1) f) (up x0 y0)) (((partial 0) f) (up x0 y0)))))
   (check-simplified? (simplify-numerical-expression
                       (/ 1 (+ (/ 1 'r1) (/ 1 'r2))))
                      '(/ (* r1 r2) (+ r1 r2)))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))