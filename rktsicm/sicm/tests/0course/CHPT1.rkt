#lang s-exp "../../main.rkt"

(require rackunit)

;; CHAPTER 1.4
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
