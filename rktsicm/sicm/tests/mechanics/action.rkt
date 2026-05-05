#lang s-exp "../../main.rkt"

(require rackunit
         "../helper+scm.rkt")

(rename-part 'derivative 'D)

(provide the-tests)
(define the-tests
  (test-suite
   "mechanics/action"
   (test-case
    "Lagrangian-action"
    ;; going from 100 -> 75 in 5 sec
    ;; constant speed
    (define (q_0 t) (- 100 (* 5 t)))
    ;; constant acceleration
    (define (q_1 t) (- 100 (* t t)))
    ;; constant acceleration with starting speed
    (define (q_2 t) (- 100 (* 5/2 t) (* 1/2 t t)))
    ;; constant deceleration with starting speed
    (define (q_3 t) (- 100 (* 15/2 t) (* -1/2 t t)))
    ;; constant D3
    (define (q_4 t) (- 100 (* 25/125 t t t)))
    ;; wait-then fall // fall then wait
    (define (q_5 t) (if (< t 4) 100 (- 100 (* 25 (- t 4)))))
    (define (q_6 t) (if (< t 1) (- 100 (* 25 t)) 75))
    (define (L state) (- (* 1/2 (expt (velocity state) 2)) (coordinate state)))
    (check-= (Lagrangian-action L q_0 0 5) -9000/24 1e-12)
    (check-= (Lagrangian-action L q_1 0 5) -9000/24 1e-12)
    (check-= (Lagrangian-action L q_2 0 5) -9125/24 1e-12)
    (check-= (Lagrangian-action L q_3 0 5) -8625/24 1e-12)
    (check-= (Lagrangian-action L q_4 0 5) -8550/24 1e-12)
    ;; due to discontinuity 5 & 6 don't integrate well if you don't break them up
    #; ;; visualize:
    (for ([F (list values (λ (f) (compose L (Gamma f)))
                   (λ (f) (λ (t) (+ (Lagrangian-action L f 0 (min 1 t))
                                    (Lagrangian-action L f 1 (min 4 (max 1 t)))
                                    (Lagrangian-action L f 4 (max 4 t))))))])
      (displayln (plot (for/list ([f (list q_0 q_1 q_2 q_3 q_4 q_5 q_6)]
                                  [i (in-naturals)])
                         (function (F f) #:color i))
                       #:x-min 0 #:x-max 5))))

   (test-case
    "linear-interpolants"
    (check-equal? (linear-interpolants 0 1 0) '())
    (check-equal? (linear-interpolants 0 1 1) '(1/2))
    (check-equal? (linear-interpolants 0 1 2) '(1/3 2/3))
    (check-equal? (linear-interpolants 0 1 3) '(1/4 2/4 3/4))
    (check-equal? (linear-interpolants 2 3 3) '(9/4 10/4 11/4))
    (check-equal? (linear-interpolants 2 4 3) '(5/2 6/2 7/2))
    (check-equal? (linear-interpolants 4 2 3) '(7/2 6/2 5/2)))
   (test-case
    "make-path"
    (check-equal? ((make-path 0 100 5 75 '(80)) 0) 100)
    (check-equal? ((make-path 0 100 5 75 '(80)) 5) 75)
    (check-equal? ((make-path 0 100 5 75 '(80)) 5/2) 80)
    (check-equal? ((make-path 0 100 5 75 '(95 90 85 80)) 0) 100)
    (check-equal? ((make-path 0 100 5 75 '(95 90 85 80)) 1) 95)
    (check-equal? ((make-path 0 100 5 75 '(95 90 85 80)) 2) 90)
    (check-equal? ((make-path 0 100 5 75 '(95 90 85 80)) 3/2) #e92.5))
   (test-case
    "parametric-path"
    (define (L state) (- (* 1/2 (expt (velocity state) 2)) (coordinate state)))
    (define pa (parametric-path-action L 0 100 5 75))
    (check-= (pa '(95 90 85 80)) -9000/24 1e-12)
    (check-= (pa '(87.5)) -9000/24 1e-12)
    (check-= (pa '(100 100 100 100)) -63.25782627865962 1e-12) ;; q_5/6 ^^see above^^
    (check-= (pa '(75 75 75 75)) 45.24911816578555 1e-12))
   (test-case
    "find-path"
    (define (L state) (- (* 1/2 (expt (velocity state) 2)) (coordinate state)))
    (check-within (simplify ((find-path L 0 100 5 75 1) 't))
                  (simplify (- 100 (* 5/2 't) (* 1/2 't 't)))
                  1e-2)
    ;; more points -> slower
    (define ~0 1e-300)
    (check-within (simplify ((find-path L 0 100 5 75 2) 't))
                  (simplify (- 100 (* 5/2 't) (* 1/2 't 't) (* ~0 't 't 't)))
                  1e-3)
    (check-within (simplify ((find-path L 0 100 5 75 3) 't))
                  (simplify (- 100 (* 5/2 't) (* 1/2 't 't) (* ~0 't 't 't) (* ~0 't 't 't 't)))
                  1e-3))
   
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