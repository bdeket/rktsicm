#lang racket/base

(require rackunit
         "../../../main.rkt"
         )

(define numerics/functions/elliptic
  (test-suite
   "numerics/functions/elliptic"
   (test-case "elliptic-integral-F"
              (define (elliptic-integral-F-check phi k)
                (definite-integral 
                  (lambda (theta) (/ 1. (sqrt (- 1. (square (* k (sin theta)))))))
                  0. phi 1.e-13))
              
              (check-within (elliptic-integral-F-check 1. .9) 1.159661070732225  1e-13)
              
              (check-within (elliptic-integral-F 1. .9)       1.159661070732199  1e-15)
              (check-within (elliptic-integral-F pi/2 .9)     2.2805491384227703 1e-15)
              (check-within (first-elliptic-integral .9)      2.2805491384227703 1e-15)
              )

   (test-case "elliptic-integral-E"
              (define (elliptic-integral-E-check phi k)
                (definite-integral 
                  (lambda (theta) (sqrt (- 1. (square (* k (sin theta))))))
                  0. phi 1.e-13))

              (check-within (elliptic-integral-E-check 1. .9) .876262219991453   1e-13)
              (check-within (elliptic-integral-E 1. .9)       .8762622199915486  1e-15)

              (check-within (complete-elliptic-integral-E .9) 1.1716970527816144 1e-15)
              (check-within (second-elliptic-integral .9)     1.171697052781614  1e-15))
   (test-case "Jacobi-elliptic"
              (check-within (elliptic-integral-F 1.1 .92)     1.3369113189159216 1e-15)
              (check-within (Jacobi-elliptic-functions 1.3369113189159216 0. list)
                            '(.9727733548546672 .231758495172875 1.) 1e-15)

              (check-within (Jacobi-elliptic-functions (elliptic-integral-F 1.1 .92) 0.92 list)
                            (list (sin 1.1) (cos 1.1)
                                  (sqrt (- 1 (* (expt .92 2) (expt (sin 1.1) 2)))))
                            1e-14)

              )
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests numerics/functions/elliptic))