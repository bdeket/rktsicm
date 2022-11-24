#lang racket/base

(require rackunit
         "../../simplify/split-poly.rkt"
         "../../simplify/simplify.rkt"
         "../../kernel-gnrc.rkt"
         "../../kernel/cstm/ghelper.rkt"
         "../../simplify/default.rkt"
         "../helper.rkt")

(define the-tests
  (test-suite
   "simplify/split-poly"
   ;;set default simplifier to nothing...
   (assign-operation 'simplify values)

   ;; make sure nothing inserted another simplifier
   (check-equal? (get-operator-record 'simplify)
                (operator-record 'simplify 1 (tree '() #t values)))
   (test-case
    "Simple test cases"
    (check-equal? (split-polynomial->expression
                   (* (square (- 'x 'y)) (cube (+ 'x 'y))))
                  '(* (expt (+ x (* -1 y)) 2) (expt (+ x y) 3)))
    (check-equal? (factor-polynomial-expression
                   (* (square (- 'x 'y)) (cube (+ 'x 'y))))
                  '(1 1 (+ x (* -1 y)) (+ x y)))
    (check-equal? (factor-polynomial-expression (square (- 'x 'y)))
                  '(1 1 (+ x (* -1 y)) 1))
    (check-equal? (factor-polynomial-expression (* 3 (cube 'z) (+ (square 'x) 'y)))
                  '(3 (+ (expt x 2) y) 1 z))
    (check-equal? (factor-polynomial-expression (* 3 (square 'z) (+ (square 'x) 'y)))
                  '(3 (+ (expt x 2) y) z 1)))
   (test-case
    "test-poly"
    (define test-poly
      (let ((x 'x) (y 'y))
        (let ((z (square (+ x (* x (expt y 2))))))
          (default-simplify
           (expression
            (* (expt (+ (cos z) y) 2)
               (expt (- (cos z) y) 3)))))))
    ; ;simplify not installed!
    (check-equal? test-poly
                  '(+ (* -1 (expt y 5))
                      (* (expt y 4)
                         (cos (+ (* (expt x 2) (expt y 4)) (* 2 (expt x 2) (expt y 2)) (expt x 2))))
                      (* 2
                         (expt y 3)
                         (expt (cos (+ (* (expt x 2) (expt y 4)) (* 2 (expt x 2) (expt y 2)) (expt x 2))) 2))
                      (* -2
                         (expt y 2)
                         (expt (cos (+ (* (expt x 2) (expt y 4)) (* 2 (expt x 2) (expt y 2)) (expt x 2))) 3))
                      (* -1
                         y
                         (expt (cos (+ (* (expt x 2) (expt y 4)) (* 2 (expt x 2) (expt y 2)) (expt x 2))) 4))
                      (expt (cos (+ (* (expt x 2) (expt y 4)) (* 2 (expt x 2) (expt y 2)) (expt x 2))) 5)))
    (check-equal? (poly:factor test-poly)
                  '(* -1
                      (expt (+ y (cos (expt (* (+ 1 (expt y 2)) x) 2))) 2)
                      (expt (+ y (* -1 (cos (expt (* (+ 1 (expt y 2)) x) 2)))) 3))))

   (test-case
    "root-out-squares1"
    ;;set default simplifier to nothing...
    (local-require )
    simplify:assign-operations
    (check-equal? (root-out-squares
                   (default-simplify (expression (sqrt (* (square (+ 'x 'y)) (cube (- 'x 'y)))))))
                  '(+ (* (expt x 2) (sqrt (+ x (* -1 y))))
                      (* -1 (expt y 2) (sqrt (+ x (* -1 y)))))))
   (test-case
    "root-out-square2"
    (check-equal? (default-simplify
                    (root-out-squares
                     (default-simplify
                       '(/ (+ (* -1
                                 (expt R 2)
                                 (((partial 0) f)
                                  (up (* R (cos phi) (sin theta))
                                      (* R (sin phi) (sin theta))
                                      (* R (cos theta))))
                                 (cos phi)
                                 (expt (cos theta) 3)
                                 (sin theta))
                              (* -1
                                 (expt R 2)
                                 (((partial 1) f)
                                  (up (* R (cos phi) (sin theta))
                                      (* R (sin phi) (sin theta))
                                      (* R (cos theta))))
                                 (expt (cos theta) 3)
                                 (sin phi)
                                 (sin theta))
                              (* (((partial 2) f)
                                  (up (* R (cos phi) (sin theta))
                                      (* R (sin phi) (sin theta))
                                      (* R (cos theta))))
                                 (sqrt
                                  (+ (* (expt R 4) (expt (cos theta) 4))
                                     (* -2 (expt R 4) (expt (cos theta) 2))
                                     (expt R 4)))
                                 (expt (cos theta) 2)))
                           (* R (sin theta))))))
                  '(/
                    (+
                     (*
                      -1
                      R
                      (sin theta)
                      (cos phi)
                      (expt (cos theta) 3)
                      (((partial 0) f)
                       (up
                        (* R (sin theta) (cos phi))
                        (* R (sin theta) (sin phi))
                        (* R (cos theta)))))
                     (*
                      -1
                      R
                      (sin theta)
                      (sin phi)
                      (expt (cos theta) 3)
                      (((partial 1) f)
                       (up
                        (* R (sin theta) (cos phi))
                        (* R (sin theta) (sin phi))
                        (* R (cos theta)))))
                     (*
                      R
                      (expt (cos theta) 4)
                      (((partial 2) f)
                       (up
                        (* R (sin theta) (cos phi))
                        (* R (sin theta) (sin phi))
                        (* R (cos theta)))))
                     (*
                      -1
                      R
                      (expt (cos theta) 2)
                      (((partial 2) f)
                       (up
                        (* R (sin theta) (cos phi))
                        (* R (sin theta) (sin phi))
                        (* R (cos theta))))))
                    (sin theta))
                  #; ;too difficult? the sqrt is at least gone...
                  (default-simplify
                    '(+ (* -1
                           R
                           (((partial 0) f)
                            (up (* R (cos phi) (sin theta))
                                (* R (sin phi) (sin theta))
                                (* R (cos theta))))
                           (cos phi)
                           (expt (cos theta) 3))
                        (* -1
                           R
                           (((partial 1) f)
                            (up (* R (cos phi) (sin theta))
                                (* R (sin phi) (sin theta))
                                (* R (cos theta))))
                           (sin phi)
                           (expt (cos theta) 3))
                        (* -1
                           R
                           (((partial 2) f)
                            (up (* R (cos phi) (sin theta))
                                (* R (sin phi) (sin theta))
                                (* R (cos theta))))
                           (expt (cos theta) 2)
                           (sin theta))))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))