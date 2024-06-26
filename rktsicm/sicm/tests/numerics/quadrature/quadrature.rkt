#lang racket/base

(require rackunit
         "../../../numerics/quadrature/quadrature.rkt"
         (submod "../../helper.rkt" runner)
         )


(provide the-tests)
(define the-tests
  (test-suite
   "numerics/quadrature/quadrature"
   (skip ;TODO all over the place!!! ;marked as BUG
   (check-equal? (* 2
                    ((make-definite-integrator
                      #;(lambda->numerical-procedure
                       '(lambda (x) (/ (sin x) x)))
                      (λ (x) (/ (sin x) x))
                      0.0
                      :+infinity
                      .01)
                     'INTEGRAL))
                 (angle -1)))
   (test-case
    "witch"
    (define witch (λ (x) (/ 4.0 (+ 1.0 (* x x)))))
    (define integrator (make-definite-integrator))

    (integrator 'SET-METHOD! 'ROMBERG)
    (integrator 'SET-ERROR! 1e-12)
    (integrator 'SET-INTEGRAND! witch)
    (integrator 'SET-LOWER-LIMIT! 0.0)
    (integrator 'SET-UPPER-LIMIT! 1.0)
    (check-= (integrator 'INTEGRAL) 3.141592653589793 1e-13)
    )
   (test-case
    "foo"
    (define (foo n)
      (define int
        (make-definite-integrator
         (lambda (x) (expt (log (/ 1 x)) n))
         0.0
         1.0
         1e-12))
      (int 'set-method! 'open-closed)
      (int 'integral))
    (check-= (foo 0) 1.                 1e-12)
    (check-= (foo 1) .9999999999979357  1e-12)
    (check-= (foo 2) 1.9999999999979101 1e-12)
    (check-= (foo 3) 5.99999999999799   1e-12)
    (check-= (foo 4) 23.999999999997893 1e-12)
    (check-= (foo 5) 119.99999999999828 1e-12)
    )
   (test-case
    "bar"
    (define (bar)
      (define int
        (make-definite-integrator
         (lambda (x) (* (exp (- x)) (log x)))
         0.0
         :+infinity
         1e-11))
      (int 'set-method! 'open-open)
      (int 'integral))
    (check-= (bar) -.5772156648993277 1e-10))
   
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))