#lang racket/base

(require rackunit
         "../../main.rkt"
         "../helper.rkt"
         )

(provide the-tests)
(define the-tests
  (test-suite
   "calculus/ode"
   (test-case
    "Lie-D"
    ;; Lie-D  = ((((Lie-D R) F) sigma) t)
    ;; => (F (R (sigma t)))
    ;; sigma: state of system in t
    ;; F function of state
    ;; R: system derivative
    ;; TODO - is this a correct test? (same output as scmutils)
    (define (sigma t) (up t (* 'a t) (+ 3 (* 'b t t))))
    (define (F sys) (up (ref sys 0) (+ (ref sys 1) (ref sys 2)) (* (ref sys 1) (ref sys 2))))
    (define R values)
    (define (R-swap sys) (up (ref sys 0) (* 1/2 (ref sys 2)) (ref sys 1)))
    (check-equal? (expression (((Lie-D R) F) (sigma 't)))
                  '(up t (+ 3 (* a t) (* b t t)) (+ (* (+ 3 (* b t t)) a t) (* a t (+ 3 (* b t t))))))
    (check-equal? (expression (((Lie-D R-swap) F) (sigma 't)))
                  '(up t (+ (* 1/2 (+ 3 (* b t t))) (* a t))
                       (+ (* 1/2 (+ 3 (* b t t)) (+ 3 (* b t t))) (* a t a t))))
    
   )))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))