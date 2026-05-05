#lang s-exp "../../main.rkt"

(require rackunit
         "../../mechanics/rotation.rkt"
         "../helper+scm.rkt")

(rename-part 'derivative 'D)

(provide the-tests)
(define UNIT (down (up 1 0 0) (up 0 1 0) (up 0 0 1)))
(define the-tests
  (test-suite
   "mechanics/rotation"
   (test-case
    "Rx-matrix"
    (check-true (eq? Rx-matrix rotate-x-matrix))
    (check-simplified? (* (Rx-matrix 'α) (up 1 2 3))
                       (up 1
                           (+ (* 2 (cos 'α)) (* -3 (sin 'α)))
                           (+ (* 2 (sin 'α)) (* 3 (cos 'α)))))
    (check-within (* UNIT (Rx-matrix (* 1/2 :pi)))
                  (down (up 1 0 0) (up 0 0 1) (up 0 -1 0)) 1e-15))
   (test-case
    "Ry-matrix"
    (check-true (eq? Ry-matrix rotate-y-matrix))
    (check-simplified? (* (Ry-matrix 'α) (up 1 2 3))
                       (up (+ (* (cos 'α)) (* 3 (sin 'α)))
                           2
                           (+ (* -1 (sin 'α)) (* 3 (cos 'α)))))
    (check-within (* UNIT (Ry-matrix (* 1/2 :pi)))
                  (down (up 0 0 -1) (up 0 1 0) (up 1 0 0)) 1e-15))
   (test-case
    "Rz-matrix"
    (check-true (eq? Rz-matrix rotate-z-matrix))
    (check-simplified? (* (Rz-matrix 'α) (up 1 2 3))
                       (up (+ (* (cos 'α)) (* -2 (sin 'α)))
                           (+ (* (sin 'α)) (* 2 (cos 'α)))
                           3))
    (check-within (* UNIT (Rz-matrix (* 1/2 :pi)))
                  (down (up 0 1 0) (up -1 0 0) (up 0 0 1)) 1e-15))
   (test-case
    "rotate-_-tuple"
    (check-equal? (rotate-x-tuple 'α)
                  (down (up 1 0 0) (up 0 (cos 'α) (sin 'α)) (up 0 (- (sin 'α)) (cos 'α))))
    (check-equal? (rotate-y-tuple 'α)
                  (down (up (cos 'α) 0 (- (sin 'α))) (up 0 1 0) (up (sin 'α) 0 (cos 'α))))
    (check-equal? (rotate-z-tuple 'α)
                  (down (up (cos 'α) (sin 'α) 0) (up (- (sin 'α)) (cos 'α) 0)  (up 0 0 1))))
   (test-case
    "rotate-_"
    (check-simplified? ((rotate-x 'α) (up 1 2 3))
                       (up 1
                           (+ (* 2 (cos 'α)) (* -3 (sin 'α)))
                           (+ (* 2 (sin 'α)) (* 3 (cos 'α)))))
    (check-simplified? ((rotate-y 'α) (up 1 2 3))
                       (up (+ (* (cos 'α)) (* 3 (sin 'α)))
                           2
                           (+ (* -1 (sin 'α)) (* 3 (cos 'α)))))
    (check-simplified? ((rotate-z 'α) (up 1 2 3))
                       (up (+ (* (cos 'α)) (* -2 (sin 'α)))
                           (+ (* (sin 'α)) (* 2 (cos 'α)))
                           3)))
   (test-case
    "wcross->w"
    (check-equal? (wcross->w (down (up 0 1 2) (up 3 4 5) (up 6 7 8)))
                  (up 5 6 1))
    (check-equal? (wcross->w (matrix-by-rows '(0 1 2) '(3 4 5) '(6 7 8)))
                  (up 5 6 1)))
   (test-case
    "angle&axis->rotation-matrix"
    (check-within (angle&axis->rotation-matrix 3 (up 1 0 0))
                  (Rx-matrix 3) 1e-15)
    (check-within (angle&vector->rotation-matrix 3 (up 10 0 0))
                  (Rx-matrix 3) 1e-15)
    (check-within (angle&axis->rotation-matrix 3 (up -1 0 0))
                  (Rx-matrix -3) 1e-15)
    (check-within (angle&axis->rotation-matrix 3 (up 0 1 0))
                  (Ry-matrix 3) 1e-15)
    (check-within (angle&axis->rotation-matrix 3 (up 0 -1 0))
                  (Ry-matrix -3) 1e-15)
    (check-within (angle&axis->rotation-matrix 3 (up 0 0 1))
                  (Rz-matrix 3) 1e-15)
    (check-within (angle&axis->rotation-matrix 3 (up 0 0 -1))
                  (Rz-matrix -3) 1e-15)
    ;; something on the axis stays on the axis
    (check-within (* (down 2 2 2) (angle&axis->rotation-matrix (* 2/3 :pi) (v:make-unit (up 1 1 1))))
                  (down 2 2 2) 1e-15)
    (check-within (* UNIT (angle&vector->rotation-matrix (* 2/3 :pi) (up 1 1 1)))
                  (down (up 0 1 0) (up 0 0 1) (up 1 0 0)) 1e-15)
    (check-within (* UNIT (vector->rotation-matrix (* (* 2/3 :pi) (v:make-unit (up 1 1 1)))))
                  (down (up 0 1 0) (up 0 0 1) (up 1 0 0)) 1e-15)
    (check-exn #px"angle&axis->rotation-matrix: zero-vector for axis:"
               (λ () (angle&axis->rotation-matrix (* 2/3 :pi) (up 0 0 0))))
    )
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))