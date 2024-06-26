#lang s-exp "../../main.rkt"

(require rackunit
         "../helper.rkt")

(rename-part 'derivative 'D)

(provide the-tests)
(define the-tests
  (test-suite
   "mechanics/Lagrangian-transformation"
   (check-simplified? (velocity
                       ((F->C p->r)
                        (->local 't 
                                 (coordinate-tuple 'r 'phi) 
                                 (velocity-tuple 'rdot 'phidot))))
                      '(up (+ (* -1 r phidot (sin phi)) (* rdot (cos phi)))
                           (+ (* r phidot (cos phi)) (* rdot (sin phi)))))
   (test-case
    "central-polar"
    (define ((L-central-rectangular m V) local)
      (let ((q (coordinate local))
            (v (velocity local)))
        (- (* 1/2 m (square v))
           (V (sqrt (square q))))))
    (define (L-central-polar m V)
      (compose (L-central-rectangular m V)
               (F->C p->r)))
    (check-simplified? ((L-central-polar 'm (literal-function 'V))
                        (->local 't (coordinate-tuple 'r 'phi) 
                                 (velocity-tuple 'rdot 'phidot)))
                       '(+ (* 1/2 m (expt phidot 2) (expt r 2))
                           (* 1/2 m (expt rdot 2))
                           (* -1 (V r)))))
   (test-case
    "driven pendulum"
    (define ((T-pend m l g ys) local)
      (let ((t (time local))
            (theta (coordinate local))
            (thetadot (velocity local)))
        (let ((ysdot (D ys)))
          (* 1/2 m
             (+ (square (* l thetadot))
                (square (ysdot t))
                (* 2 (ysdot t) l (sin theta) thetadot))))))
    (define ((V-pend m l g ys) local)
      (let ((t (time local))
            (theta (coordinate local)))
        (* m g (- (ys t) (* l (cos theta))))))
    (define L-pend (- T-pend V-pend))
    (check-simplified? ((L-pend 'm 'l 'g (literal-function 'y_s))
                        (->local 't 'theta 'thetadot))
                       '(+ (* 1/2 (expt l 2) m (expt thetadot 2))
                           (* l m thetadot ((D y_s) t) (sin theta))
                           (* g l m (cos theta))
                           (* -1 g m (y_s t))
                           (* 1/2 m (expt ((D y_s) t) 2))))
    (check-simplified? (((Lagrange-equations
                          (L-pend 'm 'l 'g (literal-function 'y_s)))
                         (literal-function 'theta))
                        't)
                       '(+ (* g l m (sin (theta t)))
                           (* (expt l 2) m (((expt D 2) theta) t))
                           (* l m (((expt D 2) y_s) t) (sin (theta t))))))
   (test-case
    "driven pendulum - coord transform"
    (define ((Lf m g) local)
      (let ((q (coordinate local))
            (v (velocity local)))
        (let ((h (ref q 1)))
          (- (* 1/2 m (square v)) (* m g h)))))
    (define ((dp-coordinates l y_s) local)
      (let ((t (time local))
            (theta (coordinate local)))
        (let ((x (* l (sin theta)))
              (y (- (y_s t) (* l (cos theta)))))
          (coordinate-tuple x y))))
    (define (L-pend m l g y_s)
      (compose (Lf m g) 
               (F->C (dp-coordinates l y_s))))
    (check-simplified? ((L-pend 'm 'l 'g (literal-function 'y_s))
                        (->local 't 'theta 'thetadot))
                       '(+ (* 1/2 (expt l 2) m (expt thetadot 2))
                           (* l m thetadot (sin theta) ((D y_s) t))
                           (* g l m (cos theta))
                           (* -1 g m (y_s t))
                           (* 1/2 m (expt ((D y_s) t) 2))))
    (check-simplified? (((Lagrange-equations
                          (L-pend 'm 'l 'g (literal-function 'y_s)))
                         (literal-function 'theta))
                        't)
                       '(+ (* g l m (sin (theta t)))
                           (* (expt l 2) m (((expt D 2) theta) t))
                           (* l m (((expt D 2) y_s) t) (sin (theta t))))))
   (test-case
    "L3-central"
    (define ((T3-spherical m) local)
      (let ((t (time local))
            (q (coordinate local))
            (qdot (velocity local)))
        (let ((r (ref q 0))
              (theta (ref q 1))
              (phi (ref q 2))
              (rdot (ref qdot 0))
              (thetadot (ref qdot 1))
              (phidot (ref qdot 2)))
          (* 1/2 m
             (+ (square rdot)
                (square (* r thetadot))
                (square (* r (sin theta) phidot)))))))
    (define (L3-central m Vr)
      (define (Vs local)
        (let ((r (ref (coordinate local) 0)))
          (Vr r)))
      (- (T3-spherical m) Vs))
    (define ((ang-mom-z m) local)
      (let ((q (coordinate local))
            (v (velocity local)))
        (ref (cross-product q (* m v)) 2)))
    (check-simplified? ((compose (ang-mom-z 'm) (F->C s->r))
                        (->local 't 
                                 (coordinate-tuple 'r 'theta 'phi)
                                 (velocity-tuple 'rdot 'thetadot 'phidot)))
                       '(* m (expt r 2) phidot (expt (sin theta) 2)))
    (check-simplified? ((Lagrangian->energy
                         (L3-central 'm (literal-function 'V)))
                        (->local 't
                                 (coordinate-tuple 'r 'theta 'phi)
                                 (velocity-tuple 'rdot 'thetadot 'phidot)))
                       '(+ (* 1/2 m (expt r 2) (expt phidot 2) (expt (sin theta) 2))
                           (* 1/2 m (expt r 2) (expt thetadot 2))
                           (* 1/2 m (expt rdot 2))
                           (V r))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))