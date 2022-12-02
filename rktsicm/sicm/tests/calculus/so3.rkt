#lang racket/base

(require rackunit
         "../../main.rkt"
         "../../calculus/so3.rkt"
         "../helper.rkt"
         )

(rename-part 'derivative 'D)

(define tests
  (test-suite
   "calculus/so3"
   (test-case
    "basis for rotations"
    (check-simplified? ((D (lambda (xyz)
                             (((D rotate-x) 'theta)
                              ((rotate-x (- 'theta)) xyz))))
                        (up 'x 'y 'z))
                       '(down (up 0 0 0)
                              (up 0 0 1)
                              (up 0 -1 0)))
    (check-simplified? ((D (lambda (xyz)
                             (((D rotate-y) 'theta)
                              ((rotate-y (- 'theta)) xyz))))
                        (up 'x 'y 'z))
                       '(down (up 0 0 -1)
                              (up 0 0 0)
                              (up 1 0 0)))
    (check-simplified? ((D (lambda (xyz)
                             (((D rotate-z) 'theta)
                              ((rotate-z (- 'theta)) xyz))))
                        (up 'x 'y 'z))
                       '(down (up 0 1 0)
                              (up -1 0 0)
                              (up 0 0 0))))
   (test-case
    "vector fields ... spatial rotations"
    (define (equation2-x p q)
      (let ((theta (ref q 0))
            (phi (ref q 1))
            (psi (ref q 2))
            (a (ref p 0))
            (b (ref p 1))
            (c (ref p 2)))
        ((D (lambda (eps)
              (- (* (rotate-z-tuple (+ phi (* a eps)))
                    (rotate-x-tuple (+ theta (* b eps)))
                    (rotate-z-tuple (+ psi (* c eps)))
                    (rotate-z-tuple (- psi))
                    (rotate-x-tuple (- theta))
                    (rotate-z-tuple (- phi)))
                 (rotate-x-tuple eps))))
         0)))
    (check-simplified? (equation2-x (up 'a 'b 'c) (up 'theta 'phi 'psi))
                       '(down
                         (up 0
                             (+ (* c (cos theta)) a)
                             (+ (* c (sin theta) (cos phi)) (* -1 b (sin phi))))
                         (up (+ (* -1 c (cos theta)) (* -1 a))
                             0
                             (+ -1 (* c (sin theta) (sin phi)) (* b (cos phi))))
                         (up (+ (* -1 c (sin theta) (cos phi)) (* b (sin phi)))
                             (+ 1 (* -1 c (sin theta) (sin phi)) (* -1 b (cos phi)))
                             0)))
    #;(define foo-x
        (solve
         (lambda (p)
           (list->vector
            (map simplify
                 (ultra-flatten (equation2-x p (up 'theta 'phi 'psi))))))
         3 9 list))
    #;(check-simplified? (simplify ((cadr foo-x) #()))
                         '(up (/ (* -1 (sin phi) (cos theta)) (sin theta))
                              (cos phi)
                              (/ (sin phi) (sin theta))))
    (define (equation2-z p q)
      (let ((theta (ref q 0))
            (phi (ref q 1))
            (psi (ref q 2))
            (a (ref p 0))
            (b (ref p 1))
            (c (ref p 2)))
        ((D (lambda (eps)
              (- (* (rotate-z-tuple (+ phi (* a eps)))
                    (rotate-x-tuple (+ theta (* b eps)))
                    (rotate-z-tuple (+ psi (* c eps)))
                    (rotate-z-tuple (- psi))
                    (rotate-x-tuple (- theta))
                    (rotate-z-tuple (- phi)))
                 (rotate-z-tuple eps))))
         0)))
    (check-simplified? (equation2-z (up 'a 'b 'c) (up 'theta 'phi 'psi))
                       '(down
                         (up 0
                             (+ -1 (* c (cos theta)) a)
                             (+ (* c (sin theta) (cos phi)) (* -1 b (sin phi))))
                         (up (+ 1 (* -1 c (cos theta)) (* -1 a))
                             0
                             (+ (* c (sin theta) (sin phi)) (* b (cos phi))))
                         (up (+ (* -1 c (sin theta) (cos phi)) (* b (sin phi)))
                             (+ (* -1 c (sin theta) (sin phi)) (* -1 b (cos phi)))
                             0)))
    #;(define foo-z
        (solve
         (lambda (p)
           (list->vector
            (map simplify
                 (ultra-flatten (equation2-z p (up 'theta 'phi 'psi))))))
         3 9 list))
    #;(check-simplified? ((cadr foo-z) #())
                         '(up 1 0 0))
    (define (equation2-y p q)
      (let ((theta (ref q 0))
            (phi (ref q 1))
            (psi (ref q 2))
            (a (ref p 0))
            (b (ref p 1))
            (c (ref p 2)))
        ((D (lambda (eps)
              (- (* (rotate-z-tuple (+ phi (* a eps)))
                    (rotate-x-tuple (+ theta (* b eps)))
                    (rotate-z-tuple (+ psi (* c eps)))
                    (rotate-z-tuple (- psi))
                    (rotate-x-tuple (- theta))
                    (rotate-z-tuple (- phi)))
                 (rotate-y-tuple eps))))
         0)))
    (check-simplified? (equation2-y (up 'a 'b 'c) (up 'theta 'phi 'psi))
                       '(down
                         (up 0
                             (+ (* c (cos theta)) a)
                             (+ 1 (* c (sin theta) (cos phi)) (* -1 b (sin phi))))
                         (up (+ (* -1 c (cos theta)) (* -1 a))
                             0
                             (+ (* c (sin theta) (sin phi)) (* b (cos phi))))
                         (up (+ -1 (* -1 c (sin theta) (cos phi)) (* b (sin phi)))
                             (+ (* -1 c (sin theta) (sin phi)) (* -1 b (cos phi)))
                             0)))
    #;(define foo-y
        (solve
         (lambda (p)
           (list->vector
            (map simplify
                 (ultra-flatten (equation2-y p (up 'theta 'phi 'psi))))))
         3 9 list))
    #;(check-simplified? ((cadr foo-y) #())
                         '(up (/ (* (cos theta) (cos phi)) (sin theta))
                              (sin phi)
                              (/ (* -1 (cos phi)) (sin theta)))))
   (test-case
    "commutator"
    (check-simplified? (((+ (commutator e_x e_y) e_z)
                         (literal-manifold-function 'f Euler-angles))
                        (Euler-angles-chi-inverse (up 'theta 'phi 'psi)))
                       0)
    (check-simplified? (((+ (commutator e_y e_z) e_x)
                         (literal-manifold-function 'f Euler-angles))
                        (Euler-angles-chi-inverse (up 'theta 'phi 'psi)))
                       0)
    (check-simplified? (((+ (commutator e_z e_x) e_y)
                         (literal-manifold-function 'f Euler-angles))
                        (Euler-angles-chi-inverse (up 'theta 'phi 'psi)))
                       0))
   (test-case
    "structure-constants"
    (check-simplified? ((so3-dual-basis so3-vector-basis)
                        (Euler-angles-chi-inverse (up 'theta 'phi 'psi)))
                       '(up (down 1 0 0) (down 0 1 0) (down 0 0 1)))
    (check-simplified? (s:map
                        (lambda (e~k)
                          (s:map
                           (lambda (e_i)
                             (s:map
                              (lambda (e_j)
                                ((e~k (commutator e_i e_j))
                                 (Euler-angles-chi-inverse (up 'theta 'phi 'psi))))
                              so3-vector-basis))
                           so3-vector-basis))
                        so3-dual-basis)
                       '(up (down (down 0 0 0) (down 0 0 -1) (down 0 1 0))
                            (down (down 0 0 1) (down 0 0 0) (down -1 0 0))
                            (down (down 0 -1 0) (down 1 0 0) (down 0 0 0)))))
   (test-case
    "Euler velocities"
    (define gamma (compose Euler-angles-chi-inverse
                           (up (literal-function 'f^theta)
                               (literal-function 'f^phi)
                               (literal-function 'f^psi))
                           time-chi))
    (check-simplified? ((((differential gamma) d/dt)
                         (literal-manifold-function 'f Euler-angles))
                        (time-chi-inverse 't))
                       '(+ (* ((D f^phi) t)
                              (((partial 1) f) (up (f^theta t) (f^phi t) (f^psi t))))
                           (* ((D f^theta) t)
                              (((partial 0) f) (up (f^theta t) (f^phi t) (f^psi t))))
                           (* ((D f^psi) t)
                              (((partial 2) f) (up (f^theta t) (f^phi t) (f^psi t))))))
    (check-simplified? (let* ((Euler-basis-over-gamma
                               (basis->basis-over-map gamma Euler-angles-basis))
                              (1form-basis-over-gamma
                               (basis->1form-basis Euler-basis-over-gamma)))
                         (s:map (lambda (w) ((w ((differential gamma) d/dt))
                                             (time-chi-inverse 't)))
                                1form-basis-over-gamma))
                       '(up ((D f^theta) t) ((D f^phi) t) ((D f^psi) t)))
    (check-simplified? (let* ((basis-over-gamma (basis->basis-over-map gamma so3-basis))
                              (1form-basis (basis->1form-basis basis-over-gamma))
                              (vector-basis (basis->vector-basis basis-over-gamma)))
                         (s:map (lambda (w)
                                  ((w ((differential gamma) d/dt))
                                   (time-chi-inverse 't)))
                                1form-basis))
                       '(up
                         (+ (* (sin (f^phi t)) (sin (f^theta t)) ((D f^psi) t))
                            (* (cos (f^phi t)) ((D f^theta) t)))
                         (+ (* -1 (cos (f^phi t)) (sin (f^theta t)) ((D f^psi) t))
                            (* (sin (f^phi t)) ((D f^theta) t)))
                         (+ (* (cos (f^theta t)) ((D f^psi) t)) ((D f^phi) t))))
    (check-simplified? ((Euler->omega (up (literal-function 'f^theta)
                                          (literal-function 'f^phi)
                                          (literal-function 'f^psi)))
                        't)
                       '(matrix-by-rows
                         (list
                          (+ (* (sin (f^phi t)) (sin (f^theta t)) ((D f^psi) t))
                             (* (cos (f^phi t)) ((D f^theta) t))))
                         (list
                          (+ (* -1 (cos (f^phi t)) (sin (f^theta t)) ((D f^psi) t))
                             (* (sin (f^phi t)) ((D f^theta) t))))
                         (list (+ (* (cos (f^theta t)) ((D f^psi) t)) ((D f^phi) t))))))
   (test-case
    "quasivelocities"
    (check-simplified? (* (rotate-z-tuple 'phi)
                          (* (rotate-x-tuple 'theta)
                             (rotate-z-tuple 'psi)))
                       '(down
                         (up (+ (* -1 (sin psi) (cos theta) (sin phi)) (* (cos psi) (cos phi)))
                             (+ (* (sin psi) (cos theta) (cos phi)) (* (sin phi) (cos psi)))
                             (* (sin theta) (sin psi)))
                         (up (+ (* -1 (cos theta) (sin phi) (cos psi)) (* -1 (sin psi) (cos phi)))
                             (+ (* (cos theta) (cos psi) (cos phi)) (* -1 (sin psi) (sin phi)))
                             (* (sin theta) (cos psi)))
                         (up (* (sin theta) (sin phi)) (* -1 (sin theta) (cos phi)) (cos theta))))
    (check-simplified? (* (down 'e_x 'e_y 'e_z)
                          (* (rotate-z-tuple 'phi)
                             (* (rotate-x-tuple 'theta)
                                (rotate-z-tuple 'psi))))
                       '(down
                         (+ (* -1 (sin psi) (cos theta) (sin phi) e_x)
                            (* (sin psi) (cos theta) (cos phi) e_y)
                            (* (cos psi) (cos phi) e_x)
                            (* (sin phi) (cos psi) e_y)
                            (* (sin theta) (sin psi) e_z))
                         (+ (* -1 (cos theta) (sin phi) (cos psi) e_x)
                            (* (cos theta) (cos psi) (cos phi) e_y)
                            (* -1 (sin psi) (cos phi) e_x)
                            (* -1 (sin psi) (sin phi) e_y)
                            (* (sin theta) (cos psi) e_z))
                         (+ (* (sin theta) (sin phi) e_x)
                            (* -1 (sin theta) (cos phi) e_y)
                            (* (cos theta) e_z))))
    (check-simplified? ((dtheta
                         (down
                          (+ (* -1 (sin psi) (cos theta) (sin phi) e_x)
                             (* (sin psi) (cos theta) (cos phi) e_y)
                             (* (cos psi) (cos phi) e_x)
                             (* (sin phi) (cos psi) e_y)
                             (* (sin theta) (sin psi) e_z))
                          (+ (* -1 (cos theta) (sin phi) (cos psi) e_x)
                             (* (cos theta) (cos psi) (cos phi) e_y)
                             (* -1 (sin psi) (cos phi) e_x)
                             (* -1 (sin psi) (sin phi) e_y)
                             (* (sin theta) (cos psi) e_z))
                          (+ (* (sin theta) (sin phi) e_x)
                             (* -1 (sin theta) (cos phi) e_y)
                             (* (cos theta) e_z))))
                        (Euler-angles-chi-inverse
                         (up 'theta 'phi 'psi)))
                       '(down (cos psi) (* -1 (sin psi)) 0))
    (check-simplified? ((dphi
                         (down
                          (+ (* -1 (sin psi) (cos theta) (sin phi) e_x)
                             (* (sin psi) (cos theta) (cos phi) e_y)
                             (* (cos psi) (cos phi) e_x)
                             (* (sin phi) (cos psi) e_y)
                             (* (sin theta) (sin psi) e_z))
                          (+ (* -1 (cos theta) (sin phi) (cos psi) e_x)
                             (* (cos theta) (cos psi) (cos phi) e_y)
                             (* -1 (sin psi) (cos phi) e_x)
                             (* -1 (sin psi) (sin phi) e_y)
                             (* (sin theta) (cos psi) e_z))
                          (+ (* (sin theta) (sin phi) e_x)
                             (* -1 (sin theta) (cos phi) e_y)
                             (* (cos theta) e_z))))
                        (Euler-angles-chi-inverse
                         (up 'theta 'phi 'psi)))
                       '(down (/ (sin psi) (sin theta)) (/ (cos psi) (sin theta)) 0))
    (check-simplified? ((dpsi
                         (down
                          (+ (* -1 (sin psi) (cos theta) (sin phi) e_x)
                             (* (sin psi) (cos theta) (cos phi) e_y)
                             (* (cos psi) (cos phi) e_x)
                             (* (sin phi) (cos psi) e_y)
                             (* (sin theta) (sin psi) e_z))
                          (+ (* -1 (cos theta) (sin phi) (cos psi) e_x)
                             (* (cos theta) (cos psi) (cos phi) e_y)
                             (* -1 (sin psi) (cos phi) e_x)
                             (* -1 (sin psi) (sin phi) e_y)
                             (* (sin theta) (cos psi) e_z))
                          (+ (* (sin theta) (sin phi) e_x)
                             (* -1 (sin theta) (cos phi) e_y)
                             (* (cos theta) e_z))))
                        (Euler-angles-chi-inverse
                         (up 'theta 'phi 'psi)))
                       '(down (/ (* -1 (cos theta) (sin psi)) (sin theta))
                              (/ (* -1 (cos psi) (cos theta)) (sin theta))
                              1)))
   (test-case
    "commutators2"
    (check-simplified? (((- (commutator ep_x ep_y) ep_z)
                         (literal-manifold-function 'f Euler-angles))
                        (Euler-angles-chi-inverse
                         (up 'theta 'phi 'psi)))
                       0)
    (check-simplified? (((- (commutator ep_y ep_z) ep_x)
                         (literal-manifold-function 'f Euler-angles))
                        (Euler-angles-chi-inverse
                         (up 'theta 'phi 'psi)))
                       0)
    (check-simplified? (((- (commutator ep_z ep_x) ep_y)
                         (literal-manifold-function 'f Euler-angles))
                        (Euler-angles-chi-inverse
                         (up 'theta 'phi 'psi)))
                       0))
   (test-case
    "structure-constants"
    (check-simplified? (s:map
                        (lambda (e~k)
                          (s:map
                           (lambda (e_i)
                             (s:map
                              (lambda (e_j)
                                ((e~k (commutator e_i e_j))
                                 (Euler-angles-chi-inverse
                                  (up 'theta 'phi 'psi))))
                              so3p-vector-basis))
                           so3p-vector-basis))
                        so3p-dual-basis)
                       '(up (down (down 0 0 0) (down 0 0 1) (down 0 -1 0))
                            (down (down 0 0 -1) (down 0 0 0) (down 1 0 0))
                            (down (down 0 1 0) (down -1 0 0) (down 0 0 0)))))
   (test-case
    "quasivelocities on the so3p basis"
    (check-simplified? (let* ((gamma (compose
                                      Euler-angles-chi-inverse
                                      (up (literal-function 'f^theta)
                                          (literal-function 'f^phi)
                                          (literal-function 'f^psi))
                                      time-chi))
                              (basis-over-gamma (basis->basis-over-map gamma so3p-basis))
                              (1form-basis (basis->1form-basis basis-over-gamma))
                              (vector-basis (basis->vector-basis basis-over-gamma)))
                         (s:map (lambda (w)
                                  ((w ((differential gamma) d/dt))
                                   (time-chi-inverse 't)))
                                1form-basis))
                       '(up
                         (+ (* ((D f^phi) t) (sin (f^theta t)) (sin (f^psi t)))
                            (* (cos (f^psi t)) ((D f^theta) t)))
                         (+ (* ((D f^phi) t) (sin (f^theta t)) (cos (f^psi t)))
                            (* -1 (sin (f^psi t)) ((D f^theta) t)))
                         (+ (* ((D f^phi) t) (cos (f^theta t))) ((D f^psi) t)))))
   (test-case
    "compare to omega body"
    (check-simplified? ((Euler->omega-body
                         (up (literal-function 'theta)
                             (literal-function 'phi)
                             (literal-function 'psi))) 't)
                       '(matrix-by-rows
                         (list
                          (+ (* (sin (theta t)) (sin (psi t)) ((D phi) t))
                             (* ((D theta) t) (cos (psi t)))))
                         (list
                          (+ (* (sin (theta t)) (cos (psi t)) ((D phi) t))
                             (* -1 ((D theta) t) (sin (psi t)))))
                         (list (+ (* (cos (theta t)) ((D phi) t)) ((D psi) t)))))
    (check-simplified? (-
                        (*
                         ((so3-vector-basis
                           (literal-manifold-function 'f Euler-angles))
                          (Euler-angles-chi-inverse
                           (up 'theta 'phi 'psi)))
                         (* (rotate-z-tuple 'phi)
                            (* (rotate-x-tuple 'theta)
                               (rotate-z-tuple 'psi))))
                        ((so3p-vector-basis
                          (literal-manifold-function 'f Euler-angles))
                         (Euler-angles-chi-inverse
                          (up 'theta 'phi 'psi))))
                       '(down 0 0 0)))
   (test-case
    "alternate angles 1"
    (define ((equation0 vartheta varphi varpsi) p)
      (let ((a (ref p 0)) (b (ref p 1)) (c (ref p 2)))
        (let ((M ((D (lambda (t) (- (* (Angles->M (up (+ vartheta (* a t))
                                                      (+ varphi (* b t))
                                                      (+ varpsi (* c t))))
                                       (Angles->M^-1 (up vartheta varphi varpsi)))
                                    (rotate-x-tuple t))))
                  0)))
          (up (ref M 0 1)
              (ref M 0 2)
              (ref M 1 2)))))
    (check-simplified? ((equation0 'vartheta 'varphi 'varpsi) (up 'a 'b 'c))
                       '(up (+ (* c (sin vartheta)) b)
                            (+ (* -1 c (cos varphi) (cos vartheta)) (* -1 a (sin varphi)))
                            (+ -1 (* -1 c (cos vartheta) (sin varphi)) (* a (cos varphi)))))
    #;(check-simplified? ((cadr (solve (equation0 'vartheta 'varphi 'varpsi) 3 3 list)) #())
                         '(up (cos varphi)
                              (* (tan vartheta) (sin varphi))
                              (/ (* -1 (sin varphi)) (cos vartheta))))
    (define ((equation1 vartheta varphi varpsi) p)
      (let ((a (ref p 0)) (b (ref p 1)) (c (ref p 2)))
        (let ((M ((D (lambda (t) (- (* (Angles->M (up (+ vartheta (* a t))
                                                      (+ varphi (* b t))
                                                      (+ varpsi (* c t))))
                                       (Angles->M^-1 (up vartheta varphi varpsi)))
                                    (rotate-y-tuple t))))
                  0)))
          (up (ref M 0 1)
              (ref M 0 2)
              (ref M 1 2)))))
    #;(check-simplified? ((cadr (solve (equation1 'vartheta 'varphi 'varpsi) 3 3 list)) #())
                         '(up (sin varphi)
                              (* -1 (tan vartheta) (cos varphi))
                              (/ (cos varphi) (cos vartheta))))
    (define ((equation2 vartheta varphi varpsi) p)
      (let ((a (ref p 0)) (b (ref p 1)) (c (ref p 2)))
        (let ((M ((D (lambda (t) (- (* (Angles->M (up (+ vartheta (* a t))
                                                      (+ varphi (* b t))
                                                      (+ varpsi (* c t))))
                                       (Angles->M^-1 (up vartheta varphi varpsi)))
                                    (rotate-z-tuple t))))
                  0)))
          (up (ref M 0 1)
              (ref M 0 2)
              (ref M 1 2)))))
    #;(check-simplified? ((cadr (solve (equation2 'vartheta 'varphi 'varpsi) 3 3 list)) #())
                         '(up 0 1 0))
    'fake-done-to-not-end-on-define)
   (test-case
    "alternate angles - commutator"
    (check-simplified? (((+ (commutator ea_x ea_y) ea_z)
                         (literal-manifold-function 'f alternate-angles))
                        (alternate-angles-chi-inverse
                         (up 'vartheta 'varphi 'varpsi)))
                       0)
    (check-simplified? (((+ (commutator ea_y ea_z) ea_x)
                         (literal-manifold-function 'f alternate-angles))
                        (alternate-angles-chi-inverse
                         (up 'vartheta 'varphi 'varpsi)))
                       0)
    (check-simplified? (((+ (commutator ea_z ea_x) ea_y)
                         (literal-manifold-function 'f alternate-angles))
                        (alternate-angles-chi-inverse
                         (up 'vartheta 'varphi 'varpsi)))
                       0)
    (check-simplified? ((so3a-dual-basis so3a-vector-basis)
                        (alternate-angles-chi-inverse
                         (up 'vartheta 'varphi 'varpsi)))
                       '(up (down 1 0 0) (down 0 1 0) (down 0 0 1))))
   (test-case
    "alternate angles - structure constants"
    (check-simplified? (s:map
                        (lambda (e~k)
                          (s:map
                           (lambda (e_i)
                             (s:map
                              (lambda (e_j)
                                ((e~k (commutator e_i e_j))
                                 (alternate-angles-chi-inverse
                                  (up 'vartheta 'varphi 'varpsi))))
                              so3a-vector-basis))
                           so3a-vector-basis))
                        so3a-dual-basis)
                       '(up (down (down 0 0 0) (down 0 0 -1) (down 0 1 0))
                            (down (down 0 0 1) (down 0 0 0) (down -1 0 0))
                            (down (down 0 -1 0) (down 1 0 0) (down 0 0 0)))))
   (test-case
    "alternate angles - ea_xyz"
    (check-simplified? (* (down 'ea_x 'ea_y 'ea_z)
                          (Angles->M (up 'vartheta 'varphi 'varpsi)))
                       '(down
                         (+ (* -1 ea_x (sin varpsi) (sin vartheta) (sin varphi))
                            (* ea_y (sin varpsi) (sin vartheta) (cos varphi))
                            (* ea_x (cos varpsi) (cos varphi))
                            (* ea_y (sin varphi) (cos varpsi))
                            (* -1 ea_z (cos vartheta) (sin varpsi)))
                         (+ (* -1 ea_x (cos vartheta) (sin varphi))
                            (* ea_y (cos vartheta) (cos varphi))
                            (* ea_z (sin vartheta)))
                         (+ (* ea_x (sin vartheta) (sin varphi) (cos varpsi))
                            (* -1 ea_y (sin vartheta) (cos varpsi) (cos varphi))
                            (* ea_x (sin varpsi) (cos varphi))
                            (* ea_y (sin varpsi) (sin varphi))
                            (* ea_z (cos vartheta) (cos varpsi)))))
    (check-simplified? ((dvartheta (down
                                    (+ (* -1 ea_x (sin varpsi) (sin vartheta) (sin varphi))
                                       (* ea_y (sin varpsi) (sin vartheta) (cos varphi))
                                       (* ea_x (cos varpsi) (cos varphi))
                                       (* ea_y (sin varphi) (cos varpsi))
                                       (* -1 ea_z (cos vartheta) (sin varpsi)))
                                    (+ (* -1 ea_x (cos vartheta) (sin varphi))
                                       (* ea_y (cos vartheta) (cos varphi))
                                       (* ea_z (sin vartheta)))
                                    (+ (* ea_x (sin vartheta) (sin varphi) (cos varpsi))
                                       (* -1 ea_y (sin vartheta) (cos varpsi) (cos varphi))
                                       (* ea_x (sin varpsi) (cos varphi))
                                       (* ea_y (sin varpsi) (sin varphi))
                                       (* ea_z (cos vartheta) (cos varpsi)))))
                        (alternate-angles-chi-inverse
                         (up 'vartheta 'varphi 'varpsi)))
                       '(down (cos varpsi) 0 (sin varpsi)))
    (check-simplified? ((dvarphi (down
                                  (+ (* -1 ea_x (sin varpsi) (sin vartheta) (sin varphi))
                                     (* ea_y (sin varpsi) (sin vartheta) (cos varphi))
                                     (* ea_x (cos varpsi) (cos varphi))
                                     (* ea_y (sin varphi) (cos varpsi))
                                     (* -1 ea_z (cos vartheta) (sin varpsi)))
                                  (+ (* -1 ea_x (cos vartheta) (sin varphi))
                                     (* ea_y (cos vartheta) (cos varphi))
                                     (* ea_z (sin vartheta)))
                                  (+ (* ea_x (sin vartheta) (sin varphi) (cos varpsi))
                                     (* -1 ea_y (sin vartheta) (cos varpsi) (cos varphi))
                                     (* ea_x (sin varpsi) (cos varphi))
                                     (* ea_y (sin varpsi) (sin varphi))
                                     (* ea_z (cos vartheta) (cos varpsi)))))
                        (alternate-angles-chi-inverse
                         (up 'vartheta 'varphi 'varpsi)))
                       '(down (/ (* -1 (sin varpsi)) (cos vartheta))
                              0
                              (/ (cos varpsi) (cos vartheta))))
    (check-simplified? ((dvarpsi (down
                                  (+ (* -1 ea_x (sin varpsi) (sin vartheta) (sin varphi))
                                     (* ea_y (sin varpsi) (sin vartheta) (cos varphi))
                                     (* ea_x (cos varpsi) (cos varphi))
                                     (* ea_y (sin varphi) (cos varpsi))
                                     (* -1 ea_z (cos vartheta) (sin varpsi)))
                                  (+ (* -1 ea_x (cos vartheta) (sin varphi))
                                     (* ea_y (cos vartheta) (cos varphi))
                                     (* ea_z (sin vartheta)))
                                  (+ (* ea_x (sin vartheta) (sin varphi) (cos varpsi))
                                     (* -1 ea_y (sin vartheta) (cos varpsi) (cos varphi))
                                     (* ea_x (sin varpsi) (cos varphi))
                                     (* ea_y (sin varpsi) (sin varphi))
                                     (* ea_z (cos vartheta) (cos varpsi)))))
                        (alternate-angles-chi-inverse
                         (up 'vartheta 'varphi 'varpsi)))
                       '(down (* (sin varpsi) (tan vartheta))
                              1
                              (* -1 (cos varpsi) (tan vartheta)))))
   (test-case
    "alternate angles - commutator 2"
    (check-simplified? (((- (commutator eap_x eap_y) eap_z)
                         (literal-manifold-function 'f alternate-angles))
                        (alternate-angles-chi-inverse
                         (up 'vartheta 'varphi 'varpsi)))
                       0)
    (check-simplified? (((- (commutator eap_y eap_z) eap_x)
                         (literal-manifold-function 'f alternate-angles))
                        (alternate-angles-chi-inverse
                         (up 'vartheta 'varphi 'varpsi)))
                       0)
    (check-simplified? (((- (commutator eap_z eap_x) eap_y)
                         (literal-manifold-function 'f alternate-angles))
                        (alternate-angles-chi-inverse
                         (up 'vartheta 'varphi 'varpsi)))
                       0))
   (test-case
    "alternate angles - struc const 2"
    (check-simplified? ((s:map
                         so3ap-dual-basis
                         so3ap-vector-basis)
                        (alternate-angles-chi-inverse
                         (up 'vartheta 'varphi 'varpsi)))
                       '(down (up 1 0 0) (up 0 1 0) (up 0 0 1)))
    (check-simplified? (s:map
                        (lambda (e~k)
                          (s:map
                           (lambda (e_i)
                             (s:map
                              (lambda (e_j)
                                ((e~k (commutator e_i e_j))
                                 (alternate-angles-chi-inverse
                                  (up 'vartheta 'varphi 'varpsi))))
                              so3ap-vector-basis))
                           so3ap-vector-basis))
                        so3ap-dual-basis)
                       '(up (down (down 0 0 0) (down 0 0 1) (down 0 -1 0))
                            (down (down 0 0 -1) (down 0 0 0) (down 1 0 0))
                            (down (down 0 1 0) (down -1 0 0) (down 0 0 0)))))
   (test-case
    "angular velocities"
    (check-simplified? (let ((q (up (literal-function 'vartheta)
                                    (literal-function 'varphi)
                                    (literal-function 'varpsi))))
                         (wcross->w (* (Angles->M^-1 (q 't))
                                       ((D (lambda (t) (Angles->M (q t)))) 't))))
                       '(up
                         (+ (* -1 (cos (vartheta t)) (sin (varpsi t)) ((D varphi) t))
                            (* ((D vartheta) t) (cos (varpsi t))))
                         (+ (* (sin (vartheta t)) ((D varphi) t)) ((D varpsi) t))
                         (+ (* (cos (vartheta t)) (cos (varpsi t)) ((D varphi) t))
                            (* ((D vartheta) t) (sin (varpsi t))))))
    (check-simplified? (let* ((gamma (compose
		alternate-angles-chi-inverse
                (up (literal-function 'vartheta)
                    (literal-function 'varphi)
                    (literal-function 'varpsi))
                time-chi))
                              (basis-over-gamma (basis->basis-over-map gamma so3ap-basis))
                              (1form-basis (basis->1form-basis basis-over-gamma))
                              (vector-basis (basis->vector-basis basis-over-gamma)))
                         (s:map (lambda (w)
                                  ((w ((differential gamma) d/dt))
                                   (time-chi-inverse 't)))
                                1form-basis))
                       '(up
                         (+ (* -1 (cos (vartheta t)) (sin (varpsi t)) ((D varphi) t))
                            (* ((D vartheta) t) (cos (varpsi t))))
                         (+ (* (sin (vartheta t)) ((D varphi) t)) ((D varpsi) t))
                         (+ (* (cos (vartheta t)) (cos (varpsi t)) ((D varphi) t))
                            (* ((D vartheta) t) (sin (varpsi t)))))))
   (test-case
    "SO3 theorem"
    (define-coordinates (up x y z) R3-rect)

    (define Jz (- (* x d/dy) (* y d/dx)))
    (define Jx (- (* y d/dz) (* z d/dy)))
    (define Jy (- (* z d/dx) (* x d/dz)))
    (check-simplified? (- (series:sum
                           (((exp (* 'alpha D))
                             (lambda (alpha)
                               (* (Euler->M
                                   (series:sum
                                    (((exp (* alpha e_z)) Euler-angles-chi)
                                     ((point Euler-angles) (up 'theta 'phi 'psi)))
                                    1))
                                  (up 'x 'y 'z))))
                            0)
                           5)
                          (series:sum
                           (((exp (* 'alpha Jz)) (chart R3-rect)) 
                            ((point R3-rect)
                             (* (Euler->M (up 'theta 'phi 'psi))
                                (up 'x 'y 'z))))
                           5))
                       '(up 0 0 0))
    (check-simplified? (- (series:sum
                           (((exp (* 'alpha D))
                             (lambda (alpha)
                               (* (Euler->M
                                   (series:sum
                                    (((exp (* alpha e_x)) Euler-angles-chi)
                                     ((point Euler-angles) (up 'theta 'phi 'psi)))
                                    4))
                                  (up 'x 'y 'z))))
                            0)
                           3)
                          (series:sum
                           (((exp (* 'alpha Jx)) (chart R3-rect)) 
                            ((point R3-rect)
                             (* (Euler->M (up 'theta 'phi 'psi))
                                (up 'x 'y 'z))))
                           3))
                       '(up 0 0 0)))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests tests))