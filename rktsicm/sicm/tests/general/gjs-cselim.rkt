#lang racket/base

(require rackunit
         "../../general/gjs-cselim.rkt"
         "../../general/list-utils.rkt"
         "../helper.rkt")

(define the-tests
  (test-suite
   "general/gjs-cselim"
   (check-unique-match? (gjs/cselim '(+ (* x 3) (- x y) (* x 3)))
                        (G306)
                        `(let ((,G306 (* x 3)))
                           (+ ,G306 (- x y) ,G306)))
   (check-unique-match? (gjs/cselim
                         '(lambda (x)
                            (/ (+ (* x 3) (- y z) (- x y) (* x 3))
                               (- y z))))
                        (G300 G301)
                        `(let ((,G301 (- y z)))
                           (lambda (x)
                             (let ((,G300 (* x 3)))
                               (/ (+ ,G300 ,G301 (- x y) ,G300)
                                  ,G301)))))
   (check-unique-match? (gjs/cselim
                         '(let ((x 32) (y 5))
                            (+ (* x 3) (- x y) (* x 3))))
                        (G296)
                        `(let ((x 32) (y 5))
                           (let ((,G296 (* x 3)))
                             (+ ,G296 (- x y) ,G296))))
   (check-unique-match? (gjs/cselim
                         '(up
                           (+ (/ (* 1/3 GM (expt dt 3) p_r_0) (* (expt m 2) (expt r_0 3)))
                              (/ (* -1/2 (expt dt 3) (expt p_phi_0 2) p_r_0) (* (expt m 3) (expt r_0 4))))
                           (+ (/ (* (expt dt 3) p_phi_0 (expt p_r_0 2)) (* (expt m 3) (expt r_0 4)))
                              (/ (* 1/3 GM (expt dt 3) p_phi_0) (* (expt m 2) (expt r_0 5)))
                              (/ (* -1/3 (expt dt 3) (expt p_phi_0 3)) (* (expt m 3) (expt r_0 6)))))
                         (fringe-smaller-than? 7))
                        (G44125 G44128)
                        `(let ((,G44125 (expt dt 3)) (,G44128 (* (expt m 3) (expt r_0 4))))
                           (up
                            (+ (/ (* 1/3 GM (expt dt 3) p_r_0) (* (expt m 2) (expt r_0 3)))
                               (/ (* -1/2 ,G44125 (expt p_phi_0 2) p_r_0) ,G44128))
                            (+ (/ (* ,G44125 p_phi_0 (expt p_r_0 2)) ,G44128)
                               (/ (* 1/3 GM (expt dt 3) p_phi_0) (* (expt m 2) (expt r_0 5)))
                               (/ (* -1/3 ,G44125 (expt p_phi_0 3)) (* (expt m 3) (expt r_0 6)))))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))