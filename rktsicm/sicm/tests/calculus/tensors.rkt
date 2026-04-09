#lang racket/base

(require rackunit
         "../../main.rkt"
         "../helper+scm.rkt"
         )

(provide the-tests)
(define the-tests
  (test-suite
   "calculus/tensors"
   (test-case
    "literal-field"
    (check-unique-match? (expression (((literal-field R2-rect) 'function) ((R2-rect '->point) #(x y))))
                         (g)
                         `(,g (up x y)))
    (check-unique-match? (expression (((literal-field R2-rect) 'scalar) ((R2-rect '->point) #(x y))))
                         (g)
                         `(,g (up x y)))
    (check-unique-match? (expression ((((literal-field R2-rect) 'up) (R2-rect '->coords)) ((R2-rect '->point) #(x y))))
                         (dx dy)
                         `(up (,dx (up x y)) (,dy (up x y))))
    (check-unique-match? (expression ((((literal-field R2-rect) 'vector) (R2-rect '->coords)) ((R2-rect '->point) #(x y))))
                         (dx dy)
                         `(up (,dx (up x y)) (,dy (up x y))))
    (check-unique-match? (expression ((((literal-field R2-rect) 'down) (literal-vector-field 'X R2-rect)) ((R2-rect '->point) #(x y))))
                         (ox oy)
                         `(+ (* (,ox (up x y)) (X^0 (up x y))) (* (,oy (up x y)) (X^1 (up x y)))))
    (check-unique-match? (expression ((((literal-field R2-rect) '1form) (literal-vector-field 'X R2-rect)) ((R2-rect '->point) #(x y))))
                         (ox oy)
                         `(+ (* (,ox (up x y)) (X^0 (up x y))) (* (,oy (up x y)) (X^1 (up x y)))))
    (check-unique-match? (expression ((((literal-field R2-rect) 'one-form) (literal-vector-field 'X R2-rect)) ((R2-rect '->point) #(x y))))
                         (ox oy)
                         `(+ (* (,ox (up x y)) (X^0 (up x y))) (* (,oy (up x y)) (X^1 (up x y)))))
    (check-exn #px"Bad type list" (λ () ((literal-field R1-rect) 'bad-type))))
   (test-case
    "tensor-field-test"
    ;; types should be either 1-form or vector
    (define ((1T X) point) (* 3 ((R1-rect '->coords) point)))
    (check-unique-match? (expression (tensor-test 1T '(1form) R1-rect))
                         (F x)
                         `((- (* 3 ,x) (+ (* 3 (,F ,x) ,x) (* 3 ,x)))))
    (check-unique-match? (expression (tensor-test 1T '(vector) R1-rect))
                         (F x)
                         `((- (* 3 ,x) (+ (* 3 (,F ,x) ,x) (* 3 ,x)))))
    (define ((2T X Y) point)
      (define coord ((R2-rect '->coords) point))
      (- (* 2 (ref coord 1)) (* 3 (ref coord 0))))
    (check-unique-match? (expression (tensor-test 2T '(1form vector) R2-rect))
                         (F x0 y0 x1 y1)
                         `((- (+ (* 2 ,y0) (* 3 ,x0))
                              (+ (* 3 ,x0) (+ (* (,F (up ,x0 ,y0)) (- (* 2 ,y0) (* 3 ,x0)))
                                              (* 2 ,y0))))
                           (-
                            (+ (* 2 ,y1) (* 3 ,x1))
                            (+ (* 3 ,x1) (+ (* (,F (up ,x1 ,y1)) (- (* 2 ,y1) (* 3 ,x1)))
                                            (* 2 ,y1))))))
    (check-unique-match? (expression (tensor-test 2T '(vector 1form) R2-rect))
                         (F x0 y0 x1 y1)
                         `((- (+ (* 2 ,y0) (* 3 ,x0))
                              (+ (* 3 ,x0) (+ (* (,F (up ,x0 ,y0)) (- (* 2 ,y0) (* 3 ,x0)))
                                              (* 2 ,y0))))
                           (-
                            (+ (* 2 ,y1) (* 3 ,x1))
                            (+ (* 3 ,x1) (+ (* (,F (up ,x1 ,y1)) (- (* 2 ,y1) (* 3 ,x1)))
                                            (* 2 ,y1)))))))
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