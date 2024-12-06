#lang racket/base

(require rackunit
         "../../generic.rkt"
         "../helper.rkt"
         )

(provide the-tests)
(define the-tests
  (test-suite
   "kernel/quaternion"
   (test-case "ORIG:quaternion->angle-axis"
              (check-equal?
               (simplify
                (quaternion->angle-axis
                 (angle-axis->quaternion 'theta
                                         (up 'x 'y (sqrt (- 1 (square 'x) (square 'y)))))))
               '(theta (up x y (sqrt (+ 1 (* -1 (expt x 2)) (* -1 (expt y 2)))))))
              )
   
   ;TODO: this test currently fails, but the original notes mention
   ;some assumptions that are made (shownotes)
   ;maybe this example is incomplete?
   (test-case "ORIG:rotation-matrix->quaternion"
              (check-equal?
               (simplify
                (let* ([theta 'theta]
                       [v (up 'x 'y 'z)]
                       [axis (v:make-unit v)]
                       [result
                        ((compose quaternion->angle-axis
                                  rotation-matrix->quaternion
                                  quaternion->rotation-matrix
                                  angle-axis->quaternion)
                         theta axis)])
                  (up (- (car result) theta)
                      (- (cadr result) axis))))
               '(up 0 (up 0 0 0)))
              )

   (test-case "ORIG:rotation-matrix->quaternion"
              (check-equal?
               (simplify
                (let* ([theta -1]
                       [v (up 1 2 3)]
                       [axis (v:make-unit v)]
                       [result
                        ((compose quaternion->angle-axis
                                  rotation-matrix->quaternion
                                  quaternion->rotation-matrix
                                  angle-axis->quaternion)
                         theta axis)])
                  (up (- (car result) theta)
                      (- (cadr result) axis))))
               '(up 2.
                    (up -.5345224838248488
                        -1.0690449676496976
                        -1.6035674514745464)))
              )
   ;;***********************************************************************************************
   (test-case
    "creation + ref"
    (define Q0 (quaternion 1 2 3 4))
    (check-equal? (q:type Q0) quaternion-type-tag)
    (check-true ((q:type-predicate Q0) Q0))
    (check-true ((q:type-predicate Q0) (quaternion 4 3 2 1)))
    (check-equal? (quaternion->vector Q0) (vector 1 2 3 4))
    (check-equal? (quaternion-ref Q0 1) 2)
    (check-equal? (real&3vector->quaternion 1 #(2 3 4)) Q0)
    (check-equal? (quaternion->3vector Q0) #(2 3 4))
    (check-equal? (quaternion->real-part Q0) 1)
    (check-equal? (q:zero-like Q0) (quaternion 0 0 0 0)))
   (test-case
    "equality"
    (check-true (q:zero? (q:zero-like (quaternion 1 2 3 4))))
    (check-false (q:zero? (quaternion 1 2 3 4)))
    (check-true (q:= (quaternion 1 2 3 4) (quaternion 1. 2. 3. 4.)))
    (check-false (q:= (quaternion 1 2 3 5) (quaternion 1. 2. 3. 4.)))
    (check-true (q:inexact? (quaternion 1. 2. 3. 4.)))
    (check-false (q:inexact? (quaternion 1 2 3 4))))
   (test-case
    "+, -, *, /"
    (define Q0 (quaternion 1 2 3 4))
    (define Q1 (quaternion 2 3 5 7))
    (check-equal? (q:+ Q0 Q1) (quaternion 3 5 8 11))
    (check-equal? (q:- Q0 Q1) (quaternion -1 -1 -2 -3))
    (check-equal? (q:* Q0 Q1) (quaternion -47 8 9 16))
    (check-equal? (q:conjugate Q0) (quaternion 1 -2 -3 -4))
    (check-equal? (q:negate Q0) (quaternion -1 -2 -3 -4))
    (check-equal? (scalar*quaternion 3 Q0) (quaternion 3 6 9 12))
    (check-equal? (quaternion*scalar Q0 3) (quaternion 3 6 9 12))
    (check-equal? (quaternion/scalar Q0 3) (quaternion 1/3 2/3 1 4/3))
    (check-equal? (q:invert Q0) (quaternion 1/30 -1/15 -1/10 -2/15))
    (check-equal? (q:/ Q1 Q0) (quaternion 17/10 0 -1/10 0)))
   (test-case
    "mag / unit"
    (define Q0 (quaternion 1 2 3 4))
    (check-equal? (q:magnitude Q0) (sqrt 30))
    (check-equal? (q:make-unit Q0) (quaternion/scalar Q0 (sqrt 30)))
    (check-true (q:unit? (quaternion 1 0 0 0)))
    (skip ;;!!
     ;; not working in general if magnitude not rational?
     (check-true (q:unit? (q:make-unit Q0))))
    (check-true (q:unit? (q:make-unit (quaternion 3 0 4 0))))
    (check-true (q:unit? (q:make-unit (quaternion 1 1 1 1))))
    (check-true (q:unit? (q:make-unit (quaternion 2 3 2 8))))
    (check-false (q:unit? Q0)))
   (test-case
    "exp / log"
    (define Q0 (q:make-unit (quaternion 1 2 3 4)))
    (check-within (q:exp (quaternion 1 0 0 0)) (quaternion (exp 1) 0 0 0) 1e-15)
    (check-within (q:exp Q0) (quaternion 0.6654105197819852 0.3710110316490788 0.556516547473618 0.7420220632981576) 1e-15)
    (check-within (q:log (quaternion (exp 1) 0 0 0)) (quaternion 1 0 0 0) 1e-15)
    (check-within (q:log (q:exp Q0)) Q0 1e-15))
   (test-case
    "apply / arity"
    (define Q0 (quaternion (λ (i j k l) i)
                           (λ (i j k l) j)
                           (λ (i j k l) k)
                           (λ (i j k l) l)))
    (define Q1 (quaternion (λ (a) a)
                           (λ (a) (tan a))
                           (λ (a) (sin a))
                           (λ (a) (cos a))))
    (check-equal? (q:apply Q0 '(1 2 3 4))
                  (quaternion 1 2 3 4))
    (check-equal? (q:arity Q0) 4)
    (check-equal? (q:arity Q1) 1)
    (check-equal? (q:arity (quaternion 1 2 3 (λ (a b) a))) 2)
    (check-equal? (q:arity (quaternion 1 2 (λ (a) a) (λ (a b) a))) #f))
   (test-case
    "derivative"
    (check-equal? (expression ((q:partial-derivative (quaternion (λ (a) a)
                                                                 (λ (a) (* a a))
                                                                 (λ (a) (* a a a))
                                                                 (λ (a) (* a a a a))) '(0)) (up 't)))
                  '(quaternion
                    (up 1)
                    (up (up (+ t t)))
                    (up (up (up (+ (* (+ t t) t) (* t t)))))
                    (up (up (up (up (+ (* (+ (* (+ t t) t) (* t t)) t) (* t t t)))))))))
   (test-case
    "4x4 and 3d-rotation"
    (check-equal? (q:->4x4 (quaternion 1 2 3 4))
                  (matrix-by-rows '(1 2 3 4) '(-2 1 -4 3) '(-3 4 1 -2) '(-4 -3 2 1)))
    (check-equal? (q:4x4-> (q:->4x4 (quaternion 1 2 3 4))) (quaternion 1 2 3 4))
    (check-equal? (expression (q:angle-axis-> 'α (vector 'j 'k 'l)))
                  '(quaternion
                    (cos (/ α 2))
                    (* (sin (/ α 2)) j)
                    (* (sin (/ α 2)) k)
                    (* (sin (/ α 2)) l)))
    (check-equal? (expression (q:->angle-axis (quaternion 'i 'j 'k 'l)))
                  '((* 2 (atan (sqrt (+ (* j j) (* k k) (* l l))) i))
                    (up
                     (/ j (sqrt (+ (* j j) (* k k) (* l l))))
                     (/ k (sqrt (+ (* j j) (* k k) (* l l))))
                     (/ l (sqrt (+ (* j j) (* k k) (* l l)))))))
    (check-equal? (q:->angle-axis (quaternion 1 0 0 1) vector) (vector :pi/2 #(0 0 1)))
    (check-equal? ((q:rotate (quaternion 1 0 0 1)) #(1 1 1)) #(-2 2 2))
    (check-within (q:rotation-matrix-> (matrix-by-rows '(1 2 3 4)
                                                       '(5 2 8 9)
                                                       '(4 2 6 7)
                                                       '(1 2 6 7)))
                  (quaternion 1.5811388300841898 -0.9486832980505138 -0.15811388300841897 0.4743416490252569) 1e-15)
    (check-equal? (expression (q:rotation-matrix-> (matrix-by-rows '(a0 a1 a2 a3)
                                                                   '(b0 b1 b2 b3)
                                                                   '(c0 c1 c2 c3)
                                                                   '(d0 d1 d2 d3))))
                  '(quaternion
                    (sqrt (* 1/4 (+ 1 a0 b1 c2)))
                    (/ (* 1/4 (- c1 b2)) (sqrt (* 1/4 (+ 1 a0 b1 c2))))
                    (/ (* 1/4 (- a2 c0)) (sqrt (* 1/4 (+ 1 a0 b1 c2))))
                    (/ (* 1/4 (- b0 a1)) (sqrt (* 1/4 (+ 1 a0 b1 c2))))))
    (check-equal? (expression (q:rotation-matrix-> (matrix-by-rows '(a0 a1 a2 a3)
                                                                   `(b0 ,(- 'a0) b2 b3)
                                                                   '(c0 c1 -1 c3)
                                                                   '(d0 d1 d2 d3))))
                  '(quaternion
                    0
                    (sqrt (* 1/4 (- (+ 1 (+ 1 a0)) (- a0))))
                    (/ (* 1/4 (+ a1 b0)) (sqrt (* 1/4 (- (+ 1 (+ 1 a0)) (- a0)))))
                    (/ (* 1/4 (+ a2 c0)) (sqrt (* 1/4 (- (+ 1 (+ 1 a0)) (- a0)))))))
    (check-equal? (expression (q:rotation-matrix-> (matrix-by-rows '(-1 a1 a2 a3)
                                                                   `(b0 b1 b2 b3)
                                                                   `(c0 c1 ,(- 'b1) c3)
                                                                   '(d0 d1 d2 d3))))
                  '(quaternion
                    0 0
                    (sqrt (* 1/4 (- (+ 2 b1) (- b1))))
                    (/ (* 1/4 (+ b2 c1)) (sqrt (* 1/4 (- (+ 2 b1) (- b1)))))))
    ;; q:rotation-matrix-> has some unreachable code for when q0, q1 & q2^2 are all 0
    ;; this can only be for (a0 b1 c1) = (-1 -1 1), this also makes q3^2 numeric, bypassing
    ;; the symbolic code
    (check-equal? (expression (q:rotation-matrix-> (matrix-by-rows '(-1 a1 a2 a3)
                                                                   `(b0 -1 b2 b3)
                                                                   `(c0 c1  1 c3)
                                                                   '(d0 d1 d2 d3))))
                  '(quaternion (* 1/4 (- b0 a1)) (* 1/4 (+ a2 c0)) (* 1/4 (+ b2 c1)) 1))
    (let ([Q (q:make-unit (quaternion 1 2 3 4))])
      (check-within (q:rotation-matrix-> (q:->rotation-matrix Q)) Q 1e-15))
    (let ([Q (q:make-unit (quaternion 4 3 2 1))])
      (check-within (q:rotation-matrix-> (q:->rotation-matrix Q)) Q 1e-15))
    (let ([Q (q:make-unit (quaternion 2 3 4 1))])
      (check-within (q:rotation-matrix-> (q:->rotation-matrix Q)) Q 1e-15))
    (let ([Q (q:make-unit (quaternion 3 4 1 2))])
      (check-within (q:rotation-matrix-> (q:->rotation-matrix Q)) Q 1e-15)))
   (test-case
    "solver"
    (check-equal? (expression (solve-linear 'a (quaternion 1 2 3 4)))
                  '(quaternion (/ 1 a) (/ 2 a) (/ 3 a) (/ 4 a)))
    (check-equal? (solve-linear (quaternion 1 2 3 4) (quaternion 2 3 5 7))
                  (quaternion  17/10 0 -1/10 0))
    (check-equal? (expression (solve-linear-left 'a (quaternion 1 2 3 4)))
                  '(quaternion (/ 1 a) (/ 2 a) (/ 3 a) (/ 4 a)))
    (check-equal? (solve-linear-left (quaternion 1 2 3 4) (quaternion 2 3 5 7))
                  (quaternion  17/10 0 -1/10 0)))

   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))