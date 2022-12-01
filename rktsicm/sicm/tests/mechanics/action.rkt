#lang s-exp "../../main.rkt"

(require rackunit
         "../helper.rkt")

(rename-part 'derivative 'D)

(define the-tests
  (test-suite
   "mechanics/action"
   (test-case
    "free-particle"
    (define ((L-free-particle mass) local)
      (let ((v (velocity local)))
        (* 1/2 mass (square v))))
    (define (test-path t)
      (coordinate-tuple (+ (* 4 t) 7)
                        (+ (* 3 t) 5)
                        (+ (* 2 t) 1)))
    (check-= (Lagrangian-action (L-free-particle 3) test-path 0 10) 435. 1e-10)
    (define ((variation nu t1 t2 h) t)
      (* h (- t t1) (- t t2) (nu t)))
    (define ((varied-free-particle-action mass path nu t1 t2) h)
      (let ((dpath (variation nu t1 t2 h)))
        (Lagrangian-action (L-free-particle mass)
                           (+ path dpath)
                           t1
                           t2)))
    (check-= ((varied-free-particle-action 3.0 test-path 
                                           (coordinate-tuple sin cos square)
                                           0.0 10.0)
              0.001)
             436.29121428571443 1e-10)
    (check-within (minimize
                   (varied-free-particle-action 3.0 test-path 
                                                (coordinate-tuple sin cos square) 
                                                0.0 10.0)
                   -2.0 1.0)
                  '(-5.828670879282072e-16 435.00000000000085 5)
                  1e-10))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))