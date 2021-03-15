#lang racket/base

(require rackunit
         "../../main.rkt"
         "../../mechanics/universal.rkt"
         )

(define kernel-tests
  (test-suite
   "kernel/function"
   (test-case "ORIG:transpose"
              (let ()
                (define (transpose-defining-relation T g a)
                  ;; T is a linear transformation T:V -> W
                  ;; the transpose of T, T^t:W* -> V* 
                  ;; Forall a in V, g in W*,  g:W -> R
                  ;; (T^t(g))(a) = g(T(a)).
                  (- (((f:transpose T) g) a) (g (T a))))

                (define DTf
                  (let* ([T (literal-function 'T (-> (UP Real Real) (UP Real Real Real)))]
                         [DT (D T)])
                    (lambda (s)
                      (lambda (x)
                        (* (DT s) x)))))

                (define a (up 'a^0 'a^1))
                (define g (lambda (w) (* (down 'g_0 'g_1 'g_2) w)))
                (define s (up 'x 'y))

                (check-equal? (simplify (transpose-defining-relation (DTf s) g a))
                              0)
                (check-equal? (simplify (((f:transpose (DTf s)) g) a))
                              '(+ (* a^0 g_0 (((partial 0 0) T^0) (up x y)))
                                  (* a^0 g_1 (((partial 0 0) T^1) (up x y)))
                                  (* a^0 g_2 (((partial 0 0) T^2) (up x y)))
                                  (* a^1 g_0 (((partial 0 1) T^0) (up x y)))
                                  (* a^1 g_1 (((partial 0 1) T^1) (up x y)))
                                  (* a^1 g_2 (((partial 0 1) T^2) (up x y)))))))


   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests kernel-tests))