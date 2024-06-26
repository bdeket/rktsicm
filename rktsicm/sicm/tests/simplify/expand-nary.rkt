#lang racket/base

(require rackunit
         "../../simplify/expand-nary.rkt"
         "../helper.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "simplify/expand-nary"
   (check-equal?  (expand-nary
                   '(+ a b c d e f))
                  ;;bdk;; original was (+ f (+ e (+ d (+ c (+ b a)))))
                  ;;bdk;; but what if + or * is not commutative?
                  '(+ (+ (+ (+ (+ a b) c) d) e) f))
   (check-equal? (expand-nary
                  '(let* ((G44125 (expt dt 3)) (G44128 (* (expt m 3) (expt r_0 4))))
                     (up
                      (+ (/ (* (* 1/3 GM) (expt dt 3) p_r_0) (* (expt m 2) (expt r_0 3)))
                         (/ (* -1/2 G44125 (expt p_phi_0 2) p_r_0) G44128))
                      (+ (/ (* G44125 p_phi_0 (expt p_r_0 2)) G44128)
                         (/ (* 1/3 GM (expt dt 3) p_phi_0) (* (expt m 2) (expt r_0 5)))
                         (/ (* -1/3 G44125 (expt p_phi_0 3)) (* (expt m 3) (expt r_0 6)))))))
                 '(let* ((G44125 (expt dt 3)) (G44128 (* (expt m 3) (expt r_0 4))))
                     (up
                      (+ (/ (* (* (* 1/3 GM) (expt dt 3)) p_r_0) (* (expt m 2) (expt r_0 3)))
                         (/ (* (* (* -1/2 G44125) (expt p_phi_0 2)) p_r_0) G44128))
                      (+ (+ (/ (* (* G44125 p_phi_0) (expt p_r_0 2)) G44128)
                            (/ (* (* (* 1/3 GM) (expt dt 3)) p_phi_0) (* (expt m 2) (expt r_0 5))))
                         (/ (* (* -1/3 G44125) (expt p_phi_0 3)) (* (expt m 3) (expt r_0 6)))))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))