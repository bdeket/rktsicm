#lang racket/base

(require rackunit
         "../../../numerics/roots/zbrent.rkt"
         "../../helper.rkt"
         )


(provide the-tests)
(define the-tests
  (test-suite
   "numerics/roots/zbrent"
   (test-case
    "1"
    (define numval 0)
    (check-within (let ((v
                         (zbrent (lambda (x)
                                   (set! numval (+ numval 1))
                                   (- (sin x) 0.5))
                                 0. 1.5
                                 1e-15
                                 100)))
                    (append v (list numval)))
                  '(#t .5235987755982988 8 10)
                  1e-15))
   (test-case
    "2"
    (define n 3)
    (define numval 0)
    (check-within (let ((v
                         (zbrent (lambda (x)
                                   (set! numval (+ numval 1))
                                   (+ (* 2 x (exp (- n))) 1 (* -2 (exp (* -1 n x)))))
                                 0. 1.
                                 1e-15
                                 100)))
                    (append v (list numval)))
                  '(#t .22370545765466293 7 9)
                  1e-15))
   (test-case
    "3"
    (define n 10)
    (define numval 0)
    (check-within (let ((v
                         (zbrent (lambda (x)
                                   (set! numval (+ numval 1))
                                   (- (* (+ 1 (expt (- 1 n) 2)) x) (expt (- 1 (* n x)) 2)))
                                 0. 1.
                                 1e-15
                                 100)))
                    (append v (list numval)))
                  '(#t 9.900009998000501e-3 8 10)
                  1e-15))
   (test-case
    "4"
    (define n 5)
    (define numval 0)
    (check-within (let ((v
                         (zbrent (lambda (x)
                                   (set! numval (+ numval 1))
                                   (- (* x x) (expt (- 1 x) n)))
                                 0. 1.
                                 1e-15
                                 100)))
                    (append v (list numval)))
                  '(#t .34595481584824217 7 9)
                  1e-15))
   (test-case
    "5"
    (define n 19)
    (define a 0)
    (define b 1e-4)
    (define numval 0)
    (check-within (let ((v
                         (zbrent (lambda (x)
                                   (set! numval (+ numval 1))
                                   (+ (expt x n) (* a x) b))
                                 -3. 5.
                                 1e-15
                                 100)))
                    (append v (list numval)))
                  '(#t -.6158482110660264 23 25)
                  1e-15))
   (test-case
    "6"
    (define n 3)
    (define numval 0)
    (check-within (let ((v
                         (zbrent (lambda (x)
                                   (set! numval (+ numval 1))
                                   (expt x n))
                                 -1. 10.
                                 1e-15
                                 200)))
                    (append v (list numval)))
                  '(#t -1.4076287793739637e-16 158 160)
                  1e-15))
   (test-case
    "7"
    (define n 9)
    (define numval 0)
    (check-within (let ((v
                         (zbrent (lambda (x)
                                   (set! numval (+ numval 1))
                                   (expt x n))
                                 -1. 10.
                                 1e-15
                                 200)))
                    (append v (list numval)))
                  '(#t -1.1555192900497495e-17 147 149)
                  1e-15))
   (test-case
    "8"
    (define n 19)
    (define numval 0)
    (check-within (let ((v
                         (zbrent (lambda (x)
                                   (set! numval (+ numval 1))
                                   (expt x n))
                                 -1. 10.
                                 1e-15
                                 200)))
                    (append v (list numval)))
                  '(#t 1.4548841231758658e-16 152 154)
                  1e-15))
   (test-case
    "kepler"
    (define (kepler ecc m)
      (define 2pi (* 8 (atan 1 1)))
      (zbrent
       (lambda (e)
         (- e (* ecc (sin e)) m))
       0.0
       (* 2 (angle -1))
       1e-15
       100))
    (check-within (kepler .99 .01)
                  '(#t .3422703164917755 13)
                  1e-15))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))