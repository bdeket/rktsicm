#lang racket/base

(require rackunit
         "../../../numerics/quadrature/defint.rkt"
         "../../../numerics/quadrature/infinities.rkt"
         )


(provide the-tests)
(define the-tests
  (test-suite
   "numerics/quadrature/defint"
   (check-= (definite-integral (lambda (x) (* (exp (- x)) (log x))) 0.0 :+infinity)
            -.5772156647120303 1e-9)
   (test-case
    "foo"
    (define ε 1e-10)
    (define (foo n) (definite-integral (lambda (x) (expt (log (/ 1 x)) n)) 0.0 1.0))
    (check-= (foo 0) 1.                 ε)
    (check-= (foo 1) .9999999999979357  ε)
    (check-= (foo 2) 1.9999999999979101 ε)
    (check-= (foo 3) 5.99999999999799   ε)
    (check-= (foo 4) 23.999999999997893 (* 10 ε))
    (check-= (foo 5) 119.99999999999828 (* 100 ε))
    (check-= (foo 6) 719.9999999997759  (* 100 ε)))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))