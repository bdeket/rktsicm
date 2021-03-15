#lang racket/base

(require rackunit
         "../../main.rkt"
         "../../mechanics/universal.rkt"
         )

(define kernel-tests
  (test-suite
   "kernel/litfun"
   (test-case "ORIG:literal-function"
              (define H
                (literal-function 'H
                                  (-> (UP Real (UP* Real 2) (DOWN* Real 2)) Real)))

              (check-equal?
               (simplify 
                (((Hamilton-equations H)
                  (coordinate-tuple (literal-function 'x)
                                    (literal-function 'y))
                  (momentum-tuple (literal-function 'p_x)
                                  (literal-function 'p_y)))
                 't))
               '(up
                 0
                 (up
                  (+ (((partial 0) x) t)
                     (* -1
                        (((partial 0 2 0) H) (up t (up (x t) (y t)) (down (p_x t) (p_y t))))))
                  (+ (((partial 0) y) t)
                     (* -1
                        (((partial 0 2 1) H) (up t (up (x t) (y t)) (down (p_x t) (p_y t)))))))
                 (down
                  (+ (((partial 0) p_x) t)
                     (((partial 0 1 0) H) (up t (up (x t) (y t)) (down (p_x t) (p_y t)))))
                  (+ (((partial 0) p_y) t)
                     (((partial 0 1 1) H) (up t (up (x t) (y t)) (down (p_x t) (p_y t))))))))
              )

   (test-case "ORIG:Lagrangian"
              (define L (literal-function 'L (Lagrangian)))

              (check-equal?
               (simplify (L (->L-state 't 'x 'v)))
               '(L (up t x v)))

              (check-equal?
               (simplify ((D L) (->L-state 't 'x 'v)))
               '(down (((partial 0 0) L) (up t x v))
                      (((partial 0 1) L) (up t x v))
                      (((partial 0 2) L) (up t x v))))

              (check-equal?
               (simplify (L (->L-state 't (up 'x 'y) (up 'v_x 'v_y))))
               '(L (up t (up x y) (up v_x v_y))))

              (check-equal?
               (simplify ((D L) (->L-state 't (up 'x 'y) (up 'v_x 'v_y))))
               '(down
                 (((partial 0 0) L) (up t (up x y) (up v_x v_y)))
                 (down (((partial 0 1 0) L) (up t (up x y) (up v_x v_y)))
                       (((partial 0 1 1) L) (up t (up x y) (up v_x v_y))))
                 (down (((partial 0 2 0) L) (up t (up x y) (up v_x v_y)))
                       (((partial 0 2 1) L) (up t (up x y) (up v_x v_y)))))))

   (test-case "ORIG:Hamiltonian"
              (define H (literal-function 'H (Hamiltonian)))

              (check-equal?
               (simplify (H (->H-state 't 'x 'p)))
               '(H (up t x p)))

              (check-equal?
               (simplify ((D H) (->H-state 't 'x 'p)))
               '(down (((partial 0 0) H) (up t x p))
                      (((partial 0 1) H) (up t x p))
                      (((partial 0 2) H) (up t x p))))

              (check-equal?
               (simplify (H (->H-state 't (up 'x 'y) (down 'p_x 'p_y))))
               '(H (up t (up x y) (down p_x p_y))))

              (check-equal?
               (simplify ((D H) (->H-state 't (up 'x 'y) (down 'p_x 'p_y))))
               '(down
                 (((partial 0 0) H) (up t (up x y) (down p_x p_y)))
                 (down (((partial 0 1 0) H) (up t (up x y) (down p_x p_y)))
                       (((partial 0 1 1) H) (up t (up x y) (down p_x p_y))))
                 (up (((partial 0 2 0) H) (up t (up x y) (down p_x p_y)))
                     (((partial 0 2 1) H) (up t (up x y) (down p_x p_y)))))))


   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests kernel-tests))