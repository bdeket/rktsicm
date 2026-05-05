#lang s-exp "../../main.rkt"

(require rackunit
         "../helper+scm.rkt")

(rename-part 'derivative 'D)

(provide the-tests)
(define the-tests
  (test-suite
   "mechanics/gamma-bar"
   (test-case
    "osculating-path"
    (check-simplified? ((osculating-path (up 't0 'q0)) 't) 'q0)
    (check-simplified? ((osculating-path (up 't0 'q0 'v0)) 't)
                       (+ 'q0 (* 'v0 (- 't 't0))))
    (check-simplified? ((osculating-path (up 't0 'q0 'v0 'a0)) 't)
                       (+ 'q0 (* 'v0 (- 't 't0)) (* 1/2 'a0 (expt (- 't 't0) 2)))))
   (test-case
    "Dt"
    (check-true (operator? Dt))
    (check-equal? (operator-name Dt) 'Dt)
    (check-simplified? ((Dt (literal-function 'F (-> (UP* Real) Real))) (up 't 'q 'v))
                       '(+ (((partial 0) F) (up t q)) (* v (((partial 1) F) (up t q)))))
    (check-simplified? ((Dt (literal-function 'F (-> (UP* Real) Real))) (up 't 'q 'v 'a))
                       '(+ (((partial 0) F) (up t q v)) (* v (((partial 1) F) (up t q v))) (* a (((partial 2) F) (up t q v))))))
   (test-case
    "trim-last-argument"
    (check-equal? (trim-last-argument (up 't 'p 'v 'a)) (up 't 'p 'v)))
   (test-case
    "euler-lagrange-operator"
    (define L (literal-function 'L (-> (UP* Real) Real)))
    (check-simplified? ((Euler-Lagrange-operator L) (up 't 'q 'v 'a))
                       '(+      (((* (partial 0) (partial 2)) L) (up t q v))
                           (* v (((* (partial 1) (partial 2)) L) (up t q v)))
                           (* a (((expt (partial 2) 2) L) (up t q v)))
                           (* -1 (((partial 1) L) (up t q v)))))
    (check-eq? LE Euler-Lagrange-operator)
    (check-eq? LE Lagrange-equations-operator))
   (test-case
    "clip"
    (check-equal? ((clip-state 0) (up 1 2 3 4)) #())
    (check-equal? ((clip-state 1) (up 1 2 3 4)) #(1))
    (check-equal? ((clip-state 2) (up 1 2 3 4)) #(1 2))
    (check-equal? (clip (up 1 2 3 4)) (up 1 2 3))
    (check-equal? (clip (up 1 2 3)) (up 1 2))
    (check-equal? (clip (up 1 2)) (up 1)))
   (test-case
    "generalized-LE exn"
    (check-exn #px"Incorrect state size for Lagrange Equations:\n\tassertion failed: \\(and \\(fix:> m 3\\) \\(even\\? m\\)\\)"
               (λ () ((generalized-LE (literal-function 'L (-> (UP* Real) Real))) '('t 'q 'v 'a 'x))))
    (check-exn #px"Incorrect state size for Lagrange Equations:\n\tassertion failed: \\(and \\(fix:> m 3\\) \\(even\\? m\\)\\)"
               (λ () ((generalized-LE (literal-function 'L (-> (UP* Real) Real))) '('t 'q)))))

   (test-case
    "Dt"
    (check-simplified? ((Dt
                        (lambda (state)
                          (let ((t (time state))
                                (q (coordinate state)))
                            (square q))))
                       (up 't (up 'x 'y) (up 'vx 'vy)))
                      '(+ (* 2 vx x) (* 2 vy y)))
   (check-simplified? ((Dt (Dt (lambda (state) (coordinate state))))
                       (up 't 'x 'v 'a 'j))
                      'a)
   (check-simplified? ((Dt (Dt (lambda (state)
                                 (square (coordinate state)))))
                       (up 't 'x 'v 'a 'j))
                      '(+ (* 2 a x) (* 2 (expt v 2)))))
   (test-case
    "1"
    (define L (literal-function 'L (Lagrangian 2)))
    (check-exn exn:fail? (λ () ((Dt L) (up 't (up 'x 'y) (up 'vx 'vy)))))
    (check-simplified? ((Dt L) (up 't (up 'x 'y) (up 'vx 'vy) (up 'ax 'ay)))
                       '(+ (* ax (((partial 2 0) L) (up t (up x y) (up vx vy))))
                           (* ay (((partial 2 1) L) (up t (up x y) (up vx vy))))
                           (* vx (((partial 1 0) L) (up t (up x y) (up vx vy))))
                           (* vy (((partial 1 1) L) (up t (up x y) (up vx vy))))
                           (((partial 0) L) (up t (up x y) (up vx vy)))))
    (check-simplified? ((LE L) (up 't (up 'x 'y) (up 'vx 'vy) (up 'ax 'ay)))
                      '(down
                        (+ (* ax (((partial 2 0) ((partial 2 0) L)) (up t (up x y) (up vx vy))))
                           (* ay (((partial 2 0) ((partial 2 1) L)) (up t (up x y) (up vx vy))))
                           (* vx (((partial 1 0) ((partial 2 0) L)) (up t (up x y) (up vx vy))))
                           (* vy (((partial 1 1) ((partial 2 0) L)) (up t (up x y) (up vx vy))))
                           (* -1 (((partial 1 0) L) (up t (up x y) (up vx vy))))
                           (((partial 0) ((partial 2 0) L)) (up t (up x y) (up vx vy))))
                        (+ (* ax (((partial 2 0) ((partial 2 1) L)) (up t (up x y) (up vx vy))))
                           (* ay (((partial 2 1) ((partial 2 1) L)) (up t (up x y) (up vx vy))))
                           (* vx (((partial 1 0) ((partial 2 1) L)) (up t (up x y) (up vx vy))))
                           (* vy (((partial 1 1) ((partial 2 1) L)) (up t (up x y) (up vx vy))))
                           (* -1 (((partial 1 1) L) (up t (up x y) (up vx vy))))
                           (((partial 0) ((partial 2 1) L)) (up t (up x y) (up vx vy))))))
    )
   (test-case
    "harmonic"
    (define ((L-harmonic m k) s)
      (- (* 1/2 m (square (velocity s)))
         (* 1/2 k (square (coordinate s)))))
    (check-simplified? ((LE (L-harmonic 'm 'k))
                        (up 't 'x 'v 'a))
                       '(+ (* a m) (* k x)))
    (check-simplified? ((LE (L-harmonic 'm 'k))
                        (up 't #(x y) #(vx vy) #(ax ay)))
                       '(down (+ (* ax m) (* k x))
                              (+ (* ay m) (* k y))))
    (check-simplified? ((LE (L-harmonic 'm 'k))
                        (up 't 'x 'v 'a 'j))
                       '(+ (* a m) (* k x)))
    (check-exn exn:fail? (λ () ((LE (L-harmonic 'm 'k))
                                (up 't 'x 'v)))))
   (test-case
    "central-polar"
    (define ((L-central-polar m V) local)
      (let ((q (coordinate local))
            (qdot (velocity local)))
        (let ((r (ref q 0))
              (phi (ref q 1))
              (rdot (ref qdot 0))
              (phidot (ref qdot 1)))
          (- (* 1/2 m
                (+ (square rdot)
                   (square (* r phidot))) )
             (V r)))))
    (check-simplified? ((LE (L-central-polar 'm (literal-function 'V)))
                        (up 't
                            (up 'r 'phi)
                            (up 'rdot 'phidot)
                            (up 'rdotdot 'phidotdot)))
                       '(down (+ (* -1 m (expt phidot 2) r) (* m rdotdot) ((D V) r))
                              (+ (* 2 m phidot r rdot) (* m phidotdot (expt r 2)))))
    (check-simplified? ((compose (LE (L-central-polar 'm (literal-function 'V)))
                                 (Gamma
                                  (coordinate-tuple (literal-function 'r)
                                                    (literal-function 'phi))
                                  4))
                        't)
                       '(down
                         (+ (* -1 m (expt ((D phi) t) 2) (r t))
                            (* m (((expt D 2) r) t))
                            ((D V) (r t)))
                         (+ (* 2 m ((D r) t) ((D phi) t) (r t))
                            (* m (((expt D 2) phi) t) (expt (r t) 2))))))
   (test-case
    "L2-harmonic"
    (define ((L2harmonic m k) state)
      (let ((x (coordinate state))
            (a (acceleration state)))
        (+ (* 1/2 m x a) (* 1/2 k (square x)))))
    (check-simplified? ((generalized-LE (L2harmonic 'm 'k))
                        (up 't 'x 'v 'a 'j 'p))
                       '(+ (* a m) (* k x))))
   (test-case
    "generalize-LE"
    (check-simplified? ((generalized-LE
                         (literal-function 'L (-> (UP Real Real Real) Real)))
                        (up 't 'x 'v 'a))
                       '(+ (* a (((partial 2) ((partial 2) L)) (up t x v)))
                           (* v (((partial 1) ((partial 2) L)) (up t x v)))
                           (((partial 0) ((partial 2) L)) (up t x v))
                           (* -1 (((partial 1) L) (up t x v)))))
    (check-simplified? ((generalized-LE
                         (literal-function 'L (-> (UP Real Real Real Real) Real)))
                        (up 't 'x 'v 'a 'j 'p))
                       '(+ (* (expt a 2) (((partial 2) ((partial 2) ((partial 3) L))) (up t x v a)))
                           (* 2 a j (((partial 2) ((partial 3) ((partial 3) L))) (up t x v a)))
                           (* 2 a v (((partial 1) ((partial 2) ((partial 3) L))) (up t x v a)))
                           (* (expt j 2) (((partial 3) ((partial 3) ((partial 3) L))) (up t x v a)))
                           (* 2 j v (((partial 1) ((partial 3) ((partial 3) L))) (up t x v a)))
                           (* (expt v 2) (((partial 1) ((partial 1) ((partial 3) L))) (up t x v a)))
                           (* 2 a (((partial 0) ((partial 2) ((partial 3) L))) (up t x v a)))
                           (* a (((partial 1) ((partial 3) L)) (up t x v a)))
                           (* -1 a (((partial 2) ((partial 2) L)) (up t x v a)))
                           (* 2 j (((partial 0) ((partial 3) ((partial 3) L))) (up t x v a)))
                           (* p (((partial 3) ((partial 3) L)) (up t x v a)))
                           (* 2 v (((partial 0) ((partial 1) ((partial 3) L))) (up t x v a)))
                           (* -1 v (((partial 1) ((partial 2) L)) (up t x v a)))
                           (((partial 0) ((partial 0) ((partial 3) L))) (up t x v a))
                           (* -1 (((partial 0) ((partial 2) L)) (up t x v a)))
                           (((partial 1) L) (up t x v a)))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))