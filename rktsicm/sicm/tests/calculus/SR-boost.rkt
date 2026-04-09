#lang racket/base

(require rackunit
         "../../main.rkt"
         "../helper+scm.rkt"
         )

(define :c (*c*))

(provide the-tests)
(define the-tests
  (test-suite
   "calculus/SR-boost"
   (test-case
    "4tuple"
    (check-equal? (make-4tuple 't (up 'x 'y 'z)) (up 't 'x 'y 'z))
    (check-equal? (4tuple->ct (make-4tuple 't (up 'x 'y 'z))) 't)
    (check-equal? (4tuple->space (make-4tuple 't (up 'x 'y 'z))) (up 'x 'y 'z)))
   (test-case
    "proper-interval"
    (check-equal? (proper-time-interval (up 't 'x 'y 'z))
                  (sqrt (- (* 't 't) (+ (* 'x 'x) (* 'y 'y) (* 'z 'z)))))
    (check-equal? (proper-space-interval (up 't 'x 'y 'z))
                  (sqrt (- (+ (* 'x 'x) (* 'y 'y) (* 'z 'z)) (* 't 't)))))
   (test-case
    "general-boost"
    (check-simplified? (proper-space-interval
                        ((general-boost (up 'vx 'vy 'vz))
                         (make-4tuple 'ct (up 'x 'y 'z))))
                       (proper-space-interval
                        (make-4tuple 'ct (up 'x 'y 'z))))

    ;; TODO - simplifier can't handle (conjugate (conjugate x))
    ;; TODO - make-literal-real is not a known-real?
    (declare-known-reals 't 'x 'y 'z 'vx 'vy 'vz)
    (define V-norm (let ([v (up 'vx 'vy 'vz)]) (/ v (magnitude v))))
    (define V (* V-norm 'c))
    (define cP (up 't 'x 'y 'z))
    (check-simplified? ((general-boost2 V-norm 'c) cP)
                       (let ([√1-c² '(sqrt (+ 1 (* -1 (expt c 2))))]
                             [|V| '(sqrt (+ (expt vx 2) (expt vy 2) (expt vz 2)))])
                         `(up
                           (/ (+ (* c vx x) (* c vy y) (* c vz z) (* t ,|V|))
                              (sqrt (+ (* -1 (expt c 2) (expt vx 2)) (* -1 (expt c 2) (expt vy 2)) (* -1 (expt c 2) (expt vz 2)) (expt vx 2) (expt vy 2) (expt vz 2))))
                           (/ (+ (* c t vx ,|V|)
                                 (* -1 vx vy y ,√1-c²) (* -1 vx vz z ,√1-c²) (* (expt vy 2) x ,√1-c²) (* (expt vz 2) x ,√1-c²)
                                 (* (expt vx 2) x) (* vx vy y) (* vx vz z))
                              (+ (* (expt vx 2) ,√1-c²) (* (expt vy 2) ,√1-c²) (* (expt vz 2) ,√1-c²)))
                           (/ (+ (* c t vy ,|V|)
                                 (* (expt vx 2) y ,√1-c²) (* -1 vx vy x ,√1-c²) (* -1 vy vz z ,√1-c²) (* (expt vz 2) y ,√1-c²)
                                 (* vx vy x) (* (expt vy 2) y) (* vy vz z))
                              (+ (* (expt vx 2) ,√1-c²) (* (expt vy 2) ,√1-c²) (* (expt vz 2) ,√1-c²)))
                           (/ (+ (* c t vz ,|V|)
                                 (* (expt vx 2) z ,√1-c²) (* -1 vx vz x ,√1-c²) (* (expt vy 2) z ,√1-c²) (* -1 vy vz y ,√1-c²)
                                 (* vx vz x) (* vy vz y) (* (expt vz 2) z))
                              (+ (* (expt vx 2) ,√1-c²) (* (expt vy 2) ,√1-c²) (* (expt vz 2) ,√1-c²))))))
    ;; TODO - simplifier can't handle this
    #;(check-simplified? ((general-boost V) cP)
                         ((general-boost2 V-norm 'c) cP))
    (check-simplified? (- ((general-boost V) cP)
                          ((general-boost2 V-norm 'c) cP))
                       (up 0 0 0 0))
    ;; so check it by calculating the difference
    (check-simplified? (let ((beta (up (/ 'v^x :c) (/ 'v^y :c) (/ 'v^z :c))))
                         (- ((general-boost2 beta 0) (up 'u0 'u1 'u2 'u3))
                            (up 'u0 'u1 'u2 'u3)))
                       (up 0 0 0 0))
    (check-simplified? (- ((general-boost2 (up 1 0 0) 0) (up 'u0 'u1 'u2 'u3))
                          (up 'u0 'u1 'u2 'u3))
                       (up 0 0 0 0)))
   (test-case
    "extended-rotation"
    ;;; Check of the relation between boosts and rotations.
    (check-simplified? ((extended-rotation (rotate-x 'theta)) #(t x y z))
                       '(up t x (+ (* y (cos theta)) (* -1 z (sin theta))) (+ (* y (sin theta)) (* z (cos theta)))))
    (check-simplified? ((extended-rotation (rotate-z 'theta)) #(t x y z))
                       '(up t (+ (* x (cos theta)) (* -1 y (sin theta))) (+ (* x (sin theta)) (* y (cos theta))) z))
    (let ((beta (up 'bx 'by 'bz))
          (xi (make-4tuple 'ct (up 'x 'y 'z)))
          (R (compose
              (rotate-x 'theta)
              (rotate-y 'phi)
              (rotate-z 'psi)))
          (R-inverse (compose
                      (rotate-z (- 'psi))
                      (rotate-y (- 'phi))
                      (rotate-x (- 'theta)))))
      (check-simplified? ((general-boost beta) xi)
                         ((compose (extended-rotation R-inverse)
                                   (general-boost (R beta))
                                   (extended-rotation R))
                          xi)
                         #:timeout 15)))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))