#lang s-exp "../../main.rkt"

(require rackunit
         "../helper.rkt")

(rename-part 'derivative 'D)

(provide the-tests)
(define the-tests
  (test-suite
   "mechanics/Routhian"
   (test-case
    "Lag"
    (define ((Lag mx kx my ky) s)
      (let ((t (time s))
            (q (coordinate s))
            (v (velocity s)))
        (let ((x (ref q 0))
              (y (ref q 1))
              (vx (ref v 0))
              (vy (ref v 1)))
          (- (+ (* 1/2 mx (square vx))
                (* 1/2 my (square vy)))
             (+ (* 1/2 kx (square x))
                (* 1/2 ky (square y))
                (* x y y))))))
    (check-simplified? ((Lagrangian->Routhian (Lag 'mx 'kx 'my 'ky))
                        (up 't (up 'x 'y) (up 'vx 'py)))
                       '(+ (* 1/2 kx (expt x 2))
                           (* 1/2 ky (expt y 2))
                           (* -1/2 mx (expt vx 2))
                           (* x (expt y 2))
                           (/ (* 1/2 (expt py 2)) my)))
    (check-simplified? (((Routh-equations 
                          (Lagrangian->Routhian (Lag 'mx 'kx 'my 'ky)))
                         (literal-function 'x)
                         (literal-function 'y)
                         (literal-function 'py))
                        't)
                       '(up
                         (+ (* -1 kx (x t)) (* -1 mx (((expt D 2) x) t)) (* -1 (expt (y t) 2)))
                         (up 0
                             (+ ((D y) t) (/ (* -1 (py t)) my))
                             (+ (* ky (y t)) (* 2 (y t) (x t)) ((D py) t))))))
   (test-case
    "Lag2"
    (define ((Lag2 m k) s)
      (let ((t (time s))
            (q (coordinate s))
            (v (velocity s)))
        (let ((x (ref q 0))
              (y (ref q 1))
              (vx (ref v 0))
              (vy (ref v 1)))
          (- (+ (* 1/2 m (square vx))
                (* 1/2 m (square vy)))
             (+ (* 1/2 k (square x))
                (* 1/2 k (square y)))))))
    (check-simplified? (((Routh-equations 
                          (Lagrangian->Routhian (Lag2 'm 'k)))
                         (up (literal-function 'x0) (literal-function 'x1))
                         (up (literal-function 'y0) (literal-function 'y1))
                         (down (literal-function 'py1) (literal-function 'py1)))
                        't)
                       '(up
                         (down (+ (* -1 k (x0 t)) (* -1 m (((expt D 2) x0) t)))
                               (+ (* -1 k (x1 t)) (* -1 m (((expt D 2) x1) t))))
                         (up
                          0
                          (up (+ ((D y0) t) (/ (* -1 (py1 t)) m))
                              (+ ((D y1) t) (/ (* -1 (py1 t)) m)))
                          (down (+ (* k (y0 t)) ((D py1) t))
                                (+ (* k (y1 t)) ((D py1) t))))))
    (check-simplified? ((Routhian->acceleration
                         (Lagrangian->Routhian (Lag2 'm 'k)))
                        (up 't
                            (up (up 'x0 'x1) (up 'y0 'y1))
                            (up (up 'vx0 'vx1) (down 'py0 'py1))))
                       '(up (/ (* -1 k x0) m) (/ (* -1 k x1) m)))
    (check-simplified? ((Routhian->state-derivative
                         (Lagrangian->Routhian (Lag2 'm 'k)))
                        (up 't
                            (up (up 'x0 'x1) (up 'y0 'y1))
                            (up (up 'vx0 'vx1) (down 'py0 'py1))))
                       '(up
                         1
                         (up (up vx0 vx1)
                             (up (/ py0 m) (/ py1 m)))
                         (up (up (/ (* -1 k x0) m) (/ (* -1 k x1) m))
                             (down (* -1 k y0) (* -1 k y1)))))
    (define ((diss2 delta0 delta1) s)
      (+ (* 1/2 delta0 (square (ref s 2 0)))
         (* 1/2 delta1 (square (ref s 2 1)))))
    (check-simplified? ((Routhian->state-derivative
                         (Lagrangian->Routhian (Lag2 'm 'k))
                         (diss2 'delta0 'delta1))
                        (up 't
                            (up (up 'x0 'x1) (up 'y0 'y1))
                            (up (up 'vx0 'vx1) (down 'py0 'py1))))
                       '(up
                         1
                         (up (up vx0 vx1) (up (/ py0 m) (/ py1 m)))
                         (up
                          (up (+ (/ (* -1 delta0 vx0) m) (/ (* -1 k x0) m))
                              (+ (/ (* -1 delta0 vx1) m) (/ (* -1 k x1) m)))
                          (down (+ (* -1 k y0) (/ (* -1 delta1 py0) m))
                                (+ (* -1 k y1) (/ (* -1 delta1 py1) m))))))
    (define ((L m1 m2 V) s)
      (let ((t (time s))
            (q (coordinate s))
            (v (velocity s)))
        (let ((xy1 (ref q 0))
              (xy2 (ref q 1))
              (v1 (ref v 0))
              (v2 (ref v 1)))
          (- (+ (* 1/2 m1 (square v1))
                (* 1/2 m2 (square v2)))
             (V xy1 xy2)))))
    (check-simplified? ((Lagrangian->Routhian
                         (L 'm1 'm2
                            (literal-function 'V
                                              (-> (X (UP Real Real) (UP Real Real)) Real))))
                        (up 't
                            (up (up 'x1 'y1) (up 'x2 'y2))
                            (up (up 'v1x 'v1y) (down 'p2x 'p2y))))
                       '(+ (* -1/2 m1 (expt v1x 2))
                           (* -1/2 m1 (expt v1y 2))
                           (V (up x1 y1) (up x2 y2))
                           (/ (* 1/2 (expt p2x 2)) m2)
                           (/ (* 1/2 (expt p2y 2)) m2)))
    (check-simplified? (((Routh-equations 
                          (Lagrangian->Routhian
                           (L 'm1 'm2
                              (literal-function 'V
                                                (-> (X (UP Real Real) (UP Real Real)) Real)))))
                         (up (literal-function 'x1) (literal-function 'y1))
                         (up (literal-function 'x2) (literal-function 'y2))
                         (down (literal-function 'p2x) (literal-function 'p2y)))
                        't)
                       '(up
                         (down
                          (+ (* -1 m1 (((expt D 2) x1) t))
                             (* -1 (((partial 0 0) V) (up (x1 t) (y1 t)) (up (x2 t) (y2 t)))))
                          (+ (* -1 m1 (((expt D 2) y1) t))
                             (* -1 (((partial 0 1) V) (up (x1 t) (y1 t)) (up (x2 t) (y2 t))))))
                         (up
                          0
                          (up (+ ((D x2) t) (/ (* -1 (p2x t)) m2))
                              (+ ((D y2) t) (/ (* -1 (p2y t)) m2)))
                          (down
                           (+ ((D p2x) t) (((partial 1 0) V) (up (x1 t) (y1 t)) (up (x2 t) (y2 t))))
                           (+ ((D p2y) t) (((partial 1 1) V) (up (x1 t) (y1 t)) (up (x2 t) (y2 t)))))))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))