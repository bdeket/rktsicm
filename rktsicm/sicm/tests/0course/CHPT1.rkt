#lang s-exp "../../main.rkt"

(module+ test
(require rackunit
         plot)

;***************************************************************************************************
;* CHAPTER 1.4                                                                                     *
;***************************************************************************************************
(let ()
  (define ((L-free-particle mass) local)
    (let ([v (velocity local)])
      (* 1/2 mass (dot-product v v))))

  (define q (up (literal-function 'x)
                (literal-function 'y)
                (literal-function 'z)))

  (check-equal?
   (simplify (q 't))
   '(up (x t) (y t) (z t)))

              
  (check-equal?
   (simplify ((simple-derivative-internal
               (lambda (eps)
                 (lambda (t)
                   ((D (* cos eps)) t)))
               'e)
              't))
   '(* -1 (sin t)))

  (check-equal?
   (simplify ((D q) 't))
   '(up (((partial 0) x) t)
        (((partial 0) y) t)
        (((partial 0) z) t)))

  (check-equal?
   (simplify ((Gamma q) 't))
   '(up t
        (up (x t) (y t) (z t))
        (up (((partial 0) x) t) (((partial 0) y) t) (((partial 0) z) t))))

  (check-equal?
   (simplify ((compose (L-free-particle 'm) (Gamma q)) 't))
   '(+
     (* 1/2 m (expt (((partial 0) x) t) 2))
     (* 1/2 m (expt (((partial 0) y) t) 2))
     (* 1/2 m (expt (((partial 0) z) t) 2))))

  (define (Lagrangian-action L q t1 t2)
    (definite-integral (compose L (Gamma q)) t1 t2))

  (define (test-path t)
    (up (+ (* 4 t) 7)
        (+ (* 3 t) 5)
        (+ (* 2 t) 1)))

  (check-equal?
   (Lagrangian-action (L-free-particle 3.0) test-path 0.0 10.0)
   435.0)

  (define ((make-eta nu t1 t2) t)
    (* (- t t1) (- t t2) (nu t)))

  (define ((varied-free-particle-action mass q nu t1 t2) eps)
    (let ([eta (make-eta nu t1 t2)])
      (Lagrangian-action 	(L-free-particle mass)
                                (+ q (* eps eta))
                                t1
                                t2)))

  (check-=
   ((varied-free-particle-action 3.0 test-path
                                 (up sin cos square)
                                 0.0 10.0)
    0.001)
   436.29121428571153
   1e-10)



  (check-within
   (minimize
    (varied-free-particle-action 3.0 test-path
                                 (up sin cos square)
                                 0.0 10.0)
    -2.0 1.0)
   '(-1.5987211554602254e-14 435.0000000000237 5)
   1e-10)
  )

(let ()
  (define ((parametric-path-action Lagrangian t0 q0 t1 q1) qs)
    (let ([path (make-path t0 q0 t1 q1 qs)])
      (Lagrangian-action Lagrangian path t0 t1)))

  (define (find-path Lagrangian t0 q0 t1 q1 n)
    (let* ([initial-qs (linear-interpolants q0 q1 n)]
           [minimizing-qs (multidimensional-minimize (parametric-path-action Lagrangian
                                                                             t0 q0 t1 q1)
                                                     initial-qs)])
      (make-path t0 q0 t1 q1 minimizing-qs)))

  (define ((L-harmonic m k) local)
    (let ([q (coordinate local)]
          [v (velocity local)])
      (- (* 1/2 m (square v)) (* 1/2 k (square q)))))

  (define q1 (find-path (L-harmonic 1.0 1.0) 0.0 1.0 :pi/2 0.0 3))

  (plot (function (- q1 cos) 0.0 :pi/2))
  )
;***************************************************************************************************
;* CHAPTER 1.5                                                                                     *
;***************************************************************************************************
;exercise 1.8
(let ()
  (define ((((delta eta) f) q) t)
    (define (g epsilon) (f (+ q (* epsilon eta))))
    (((D g) 0) t))

  (define (f q)
    ;;changed from textbook: either Gamma needs to be limited to the first derivative (Gamma q 2)
    ;;or the function type for F needs to be changed to include more (standard 3) derivatives
    (compose (literal-function 'F
                               (-> (UP Real (UP* Real)) Real))
             (Gamma q 2)))

  (define q (literal-function 'q (-> Real (UP Real Real))))

  (define η (literal-function 'q (-> Real (UP Real Real))))

  (define (g q)
    (compose (literal-function 'G
                               (-> (UP Real (UP* Real)) Real))
             (Gamma q 2)))

  (check-equal?
   (simplify ((- (((delta η) (* f g)) q)
                 (+ (* (((delta η) f) q) (g q))
                    (* (f q) (((delta η) g) q))))
              't))
   0)

  (check-equal?
   (simplify ((- (((delta η) (+ f g)) q)
                 (+ (((delta η) f) q)
                    (((delta η) g) q)))
              't))
   0)

  (check-equal?
   (simplify ((- (((delta η) (* 'c f)) q)
                 (* 'c (((delta η) f) q)))
              't))
   0)

  (check-equal?
   (simplify ((- (D (((delta η) f) q))
                 (((delta η) (λ (q)(D (f q)))) q))
              't))
   0)

  )

;1.5.2
(let ()
  (define ((L-free-particle mass) local)
    (let ([v (velocity local)])
      (* 1/2 mass (dot-product v v))))

  (define ((L-harmonic m k) local)
    (let ([q (coordinate local)]
          [v (velocity local)])
      (- (* 1/2 m (square v)) (* 1/2 k (square q)))))

  (define ((lagrange-equations Lagrangian) q)
    (- (D (compose ((partial 2) Lagrangian) (Gamma q)))
       (compose ((partial 1) Lagrangian) (Gamma q))))

  (define (test-path t)
    (up (+ (* 'a t) 'a0)
        (+ (* 'b t) 'b0)
        (+ (* 'c t) 'c0)))

  (check-equal?
   (((Lagrange-equations (L-free-particle 'm))
     test-path)
    't)
   (down 0 0 0))

  (check-equal?
   (simplify
    (((Lagrange-equations (L-free-particle 'm))
      (literal-function 'x))
     't))
   '(* m (((expt (partial 0) 2) x) t)))

  (define (proposed-solution t)
    (* 'A (cos (+ (* 'omega t) 'phi))))

  (check-equal?
   (simplify
    (((Lagrange-equations (L-harmonic 'm 'k))
      proposed-solution)
     't))
   '(+
     (* -1 A m (expt omega 2) (cos (+ (* omega t) phi)))
     (*    A k                (cos (+ (* omega t) phi)))))

  (define ((L-central-polar m V) local)
    (define q (coordinate local))
    (define qdot (velocity local))
    (define r (ref q 0))
    (define phi (ref q 1))
    (define rdot (ref qdot 0))
    (define phidot (ref qdot 1))
    (- (* 1/2 m
          (+ (square rdot) (square (* r phidot))))
       (V r)))

  (define ((gravitational-energy G m1 m2) r)
    (- (/ (* G m1 m2) r)))

  (L-central-polar (/ (* 'm1 'm2) (+ 'm1 'm2))
                   (gravitational-energy 'G 'm1 'm2))
))
