#lang racket/base

(require rackunit
         "../../main.rkt"
         "../helper+scm.rkt"
         )

(provide the-tests)
(define the-tests
  (test-suite
   "calculus/interior-product"
   (test-case
    "product construction"
    (define X (literal-vector-field 'X R3-rect))
    (define-coordinates (up x y z) R3-rect)
    (define int-prod ((interior-product X) (wedge dx dy)))
    (check-true (operator? int-prod))
    (check-equal? (operator-name int-prod) '((interior-product X) (wedge dx dy)))
    (check-equal? (operator-subtype int-prod) wedge)
    (check-equal? (operator-arity int-prod) 1)
    (check-equal? (expression ((int-prod X) ((R3-rect '->point) #(x y z))))
                  '(+ (*    (X^0 (up x y z)) (X^1 (up x y z)))
                      (* -1 (X^0 (up x y z)) (X^1 (up x y z)))))
    (check-equal? (expression (((interior-product X) (wedge dz)) ((R3-rect '->point) #(x y z))))
                  '(X^2 (up x y z))))
   (test-case
    "bad arguments"
    (define X (literal-vector-field 'X R3-rect))
    (define-coordinates (up x y z) R3-rect)
    (define int-prod ((interior-product X) (wedge dx dy)))
    (check-exn #px"X not a vector field: interior-product"
               (λ () (interior-product (λ (x) (vector x (* 2 x) (* x x))))))
    (check-exn #px"alpha not a form field: interior-product"
               (λ () ((interior-product X) (λ (x) (vector x (* 2 x) (* x x))))))
    (check-exn #px"Rank of form not greater than zero: interior-product"
               (λ () ((interior-product X) (make-operator (λ (x) (vector x (* 2 x) (* x x)))
                                                          'fake-formfield
                                                          wedge
                                                          0))))
    (check-exn #px"Wrong number of arguments to interior product"
               (λ () (int-prod X X)))
    (check-exn #px"Wrong number of arguments to interior product"
               (λ () (int-prod)))
    )
   (test-case
    "Claim L_x omega = i_x d omega + d i_x omega (Cartan Homotopy Formula)"
    (define-coordinates (up x y z) R3-rect)
    (define R3-rect-point ((R3-rect '->point) (up 'x0 'y0 'z0)))
    (define X (literal-vector-field 'X R3-rect))
    (define Y (literal-vector-field 'Y R3-rect))
    (define Z (literal-vector-field 'Z R3-rect))
    (define W (literal-vector-field 'W R3-rect))
    (define alpha
      (compose (literal-function 'alpha (-> (UP Real Real Real) Real))
               (R3-rect '->coords)))
    (define beta
      (compose (literal-function 'beta (-> (UP Real Real Real) Real))
               (R3-rect '->coords)))
    (define gamma
      (compose (literal-function 'gamma (-> (UP Real Real Real) Real))
               (R3-rect '->coords)))
    (define omega
      (+ (* alpha (wedge dx dy))
         (* beta (wedge dy dz))
         (* gamma (wedge dz dx))))
    (define ((L1 X) omega)
      (+ ((interior-product X) (d omega))
         (d ((interior-product X) omega))))
    (check-simplified? ((- (((Lie-derivative X) omega) Y Z)
                           (((L1 X) omega) Y Z))
                        ((R3-rect '->point) (up 'x0 'y0 'z0)))
                       0)
    (check-simplified? (let ((omega (literal-1form-field 'omega R3-rect)))
                         ((- (((Lie-derivative X) omega) Y)
                             (((L1 X) omega) Y))
                          ((R3-rect '->point) (up 'x0 'y0 'z0))))
                       0)
    (check-simplified? (let ((omega (* alpha (wedge dx dy dz))))
                         ((- (((Lie-derivative X) omega) Y Z W)
                             (((L1 X) omega) Y Z W))
                          ((R3-rect '->point) (up 'x0 'y0 'z0))))
                       0))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))