#lang s-exp "../../main.rkt"

(require rackunit
         "../helper.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "mechanics/universal"
   (check-simplified? ((D-as-matrix (literal-function 'H (Hamiltonian 2)))
                       (up 't (up 'x 'y) (down 'p_x 'p_y)))
                      '(matrix-by-rows
                        (list (((partial 0) H) (up t (up x y) (down p_x p_y)))
                              (((partial 1 0) H) (up t (up x y) (down p_x p_y)))
                              (((partial 1 1) H) (up t (up x y) (down p_x p_y)))
                              (((partial 2 0) H) (up t (up x y) (down p_x p_y)))
                              (((partial 2 1) H) (up t (up x y) (down p_x p_y))))))
   (check-equal? (ref (Taylor-series-coefficients exp 0) 0)
                 1)
   (check-equal? (ref (Taylor-series-coefficients exp 0) 1)
                 1)
   (check-equal? (ref (Taylor-series-coefficients exp 0) 2)
                 1/2)
   (check-equal? (ref (Taylor-series-coefficients exp 0) 3)
                 1/6)
   (test-case
    "sin/cos"
    (define (f v)
      (* (sin (* 3 (ref v 0)))
         (cos (* 4 (ref v 1)))))
    (check-equal? (ref (Taylor-series-coefficients f (up 'a 'b)) 0)
                  '(* (cos (* 4 b)) (sin (* 3 a))))
    (check-equal? (ref (Taylor-series-coefficients f (up 'a 'b)) 1)
                  '(down (* 3 (cos (* 4 b)) (cos (* 3 a))) (* -4 (sin (* 3 a)) (sin (* 4 b)))))
    (check-simplified? (ref (Taylor-series-coefficients f (up 'a 'b)) 2)
                       '(down (down (* -9/2 (cos (* 4 b)) (sin (* 3 a))) (* -6 (sin (* 4 b)) (cos (* 3 a))))
                              (down (* -6 (sin (* 4 b)) (cos (* 3 a))) (* -8 (cos (* 4 b)) (sin (* 3 a))))))
    (check-simplified? (ref (Taylor-series-coefficients (literal-function 'G (-> (UP Real Real) Real))
                                                        (up 'a 'b))
                            0)
                       '(G (up a b)))
    (check-simplified? (ref (Taylor-series-coefficients (literal-function 'G (-> (UP Real Real) Real))
                                                        (up 'a 'b))
                            1)
                       '(down (((partial 0) G) (up a b)) (((partial 1) G) (up a b))))
    (check-simplified? (ref (Taylor-series-coefficients (literal-function 'G (-> (UP Real Real) Real))
                                                        (up 'a 'b))
                            2)
                       '(down
                         (down (* 1/2 (((partial 0) ((partial 0) G)) (up a b)))
                               (* 1/2 (((partial 0) ((partial 1) G)) (up a b))))
                         (down (* 1/2 (((partial 0) ((partial 1) G)) (up a b)))
                               (* 1/2 (((partial 1) ((partial 1) G)) (up a b))))))
    (check-simplified? (ref (Taylor-series-coefficients (literal-function 'H (-> (X Real Real) Real))
                                                        'a 'b)
                            0)
                       '(H a b))
    (check-simplified? (ref (Taylor-series-coefficients (literal-function 'H (-> (X Real Real) Real))
                                                        'a 'b)
                            3)
                       '(down
                         (down
                          (down (* 1/6 (((partial 0) ((partial 0) ((partial 0) H))) a b))
                                (* 1/6 (((partial 0) ((partial 0) ((partial 1) H))) a b)))
                          (down (* 1/6 (((partial 0) ((partial 0) ((partial 1) H))) a b))
                                (* 1/6 (((partial 0) ((partial 1) ((partial 1) H))) a b))))
                         (down
                          (down (* 1/6 (((partial 0) ((partial 0) ((partial 1) H))) a b))
                                (* 1/6 (((partial 0) ((partial 1) ((partial 1) H))) a b)))
                          (down (* 1/6 (((partial 0) ((partial 1) ((partial 1) H))) a b))
                                (* 1/6 (((partial 1) ((partial 1) ((partial 1) H))) a b)))))))
   ;;BUG 
   (check-simplified? ((D (lambda (y) 
                            (ref (Taylor-series-coefficients (lambda (x) (* x y)) 0) 1)))
                       'a)
                      0)
;;; This does not produce a function.  It is a 
;;; symbolic manipulation.
   (test-case
    "bug1"
    (define (Taylor-series-coefficients f x)
      (let ((dummy (gensym 'x)))
        ((series:elementwise (compose
                              simplify
                              (lambda (term)
                                (substitute x dummy term))
                              simplify))
         (((exp D) f) dummy))))
    (check-simplified? ((D (lambda (y) 
                             (ref (Taylor-series-coefficients (lambda (x) (* x y)) 0) 1)))
                        'a)
                       0))
   (test-case
    "bug2"
;;; Apparently due to simplify...
    (define (Taylor-series-coefficients f x)
      (let ((dummy (gensym 'x)))
        ((series:elementwise (lambda (term)
                               (substitute x dummy term)))
         (((exp D) f) dummy))))
    (check-simplified? ((D (lambda (y) 
                             (ref (Taylor-series-coefficients (lambda (x) (* x y)) 0) 1)))
                        'a)
                       1))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))