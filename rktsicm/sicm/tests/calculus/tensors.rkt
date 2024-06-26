#lang racket/base

(require rackunit
         "../../main.rkt"
         "../helper.rkt"
         )

(provide the-tests)
(define the-tests
  (test-suite
   "calculus/tensors"
   (check-simplified? (tensor-test
                       (Riemann (covariant-derivative (literal-Cartan 'G R3-rect)))
                       '(1form vector vector vector)
                       R3-rect)
                      '(0 0 0 0))
   (test-case
    "nabla"
    (define ((F nabla) omega u v)
      (omega ((nabla u) v)))
    (check-unique-match? (simplify (tensor-test
                                    (F (covariant-derivative (literal-Cartan 'G R3-rect)))
                                    '(1form vector vector)
                                    R3-rect))
                         (x y z
                          g
                          v^0 w^0 ω_0
                          v^1 w^1 ω_1
                          v^2 w^2 ω_2)
                         `(0
                           0
                           ;; maybe ok, in original written as <mess>
                           (+
                            (* (,v^0 (up ,x ,y ,z)) (,w^0 (up ,x ,y ,z))             (((partial 0) ,g) (up ,x ,y ,z)) (,ω_0 (up ,x ,y ,z)))
                            (* (,v^0 (up ,x ,y ,z)) (((partial 0) ,g) (up ,x ,y ,z)) (,ω_1 (up ,x ,y ,z))             (,w^1 (up ,x ,y ,z)))
                            (* (,v^0 (up ,x ,y ,z)) (((partial 0) ,g) (up ,x ,y ,z)) (,ω_2 (up ,x ,y ,z))             (,w^2 (up ,x ,y ,z)))
                            (* (,w^0 (up ,x ,y ,z)) (,v^1 (up ,x ,y ,z))             (((partial 1) ,g) (up ,x ,y ,z)) (,ω_0 (up ,x ,y ,z)))
                            (* (,w^0 (up ,x ,y ,z)) (,v^2 (up ,x ,y ,z))             (((partial 2) ,g) (up ,x ,y ,z)) (,ω_0 (up ,x ,y ,z)))
                            (* (,v^1 (up ,x ,y ,z)) (((partial 1) ,g) (up ,x ,y ,z)) (,ω_1 (up ,x ,y ,z))             (,w^1 (up ,x ,y ,z)))
                            (* (,v^1 (up ,x ,y ,z)) (((partial 1) ,g) (up ,x ,y ,z)) (,ω_2 (up ,x ,y ,z))             (,w^2 (up ,x ,y ,z)))
                            (* (,v^2 (up ,x ,y ,z)) (((partial 2) ,g) (up ,x ,y ,z)) (,ω_1 (up ,x ,y ,z))             (,w^1 (up ,x ,y ,z)))
                            (* (,v^2 (up ,x ,y ,z)) (((partial 2) ,g) (up ,x ,y ,z)) (,ω_2 (up ,x ,y ,z))             (,w^2 (up ,x ,y ,z)))))))
   (test-case
    "torsion vector"
    (define ((G nabla) omega u v)
      (omega ((torsion-vector nabla) u v)))
    (check-simplified? (tensor-test
                        (G (covariant-derivative (literal-Cartan 'G R3-rect)))
                        '(1form vector vector)
                        R3-rect)
                       '(0 0 0)))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))