#lang racket/base

(require rackunit
         "../../main.rkt"
         "../helper.rkt"
         )

(define (make-GCF R2-rect-basis)
  (define (Gijk i j k)
    (literal-manifold-function
     (string->symbol
      (string-append "G^"
                     (number->string i)
                     "_"
                     (number->string j)
                     (number->string k)))
     R2-rect))
  (define G
    (down (down (up (Gijk 0 0 0)
                    (Gijk 1 0 0))
                (up (Gijk 0 1 0)
                    (Gijk 1 1 0)))
          (down (up (Gijk 0 0 1)
                    (Gijk 1 0 1))
                (up (Gijk 0 1 1)
                    (Gijk 1 1 1)))))
  (define CG (make-Christoffel G R2-rect-basis))
  (values G CG (Christoffel->Cartan CG)))

(void (clear-arguments)
      (suppress-arguments '((up x0 y0)));<= this removes (up x0 y0) from the below simplified results
      (rename-part 'derivative 'D)) 
; ie (fct (up x0 y0)) is converted to fct

(define the-tests
  (test-suite
   "calculus/covariant-derivative"
   (test-case "ORIG:covariant-derivative 1"
              (define omega (literal-1form-field 'omega R4-rect))
              (declare-argument-types! omega (list vector-field?))
              (define m (typical-point R4-rect))
              (define X (literal-vector-field 'X R4-rect))
              (define Tomega (indexed->typed
                              (typed->indexed omega
                                              (coordinate-system->basis R4-rect))
                              (coordinate-system->basis R4-rect)))
              (define V (literal-vector-field 'V R4-rect))
              (define C (literal-Cartan 'G R4-rect))

              (check-equal?
               (simplify
                (- (((((covariant-derivative C) X) omega) V) m)
                   (((((covariant-derivative C) X) Tomega) V) m)))
               0))

   (test-case "ORIG:covariant-derivative 2"
              (define basis (coordinate-system->basis R4-rect))
              (define V (literal-vector-field 'V R4-rect))
              (define TV (lambda (1form) (1form V)))
              (declare-argument-types! TV (list 1form-field?))
              (define m (typical-point R4-rect))
              (define X (literal-vector-field 'X R4-rect))
              (define omega (literal-1form-field 'omega R4-rect))
              (define C (literal-Cartan 'G R4-rect))

              (check-equal?
               (simplify (- ((omega V) m) ((TV omega) m)))
               0)

              (check-equal?
               (simplify
                (- ((omega (((covariant-derivative C) X) V)) m)
                   (((((covariant-derivative C) X) TV) omega) m)))
               0))

   (test-case "ORIG:covariant-derivative 3"
              (define g S2-metric)
              (define G (metric->Christoffel-2 g (coordinate-system->basis S2-spherical)))
              (define C (Christoffel->Cartan G))
              (define V (literal-vector-field 'V S2-spherical))
              (define X (literal-vector-field 'X S2-spherical))
              (define Y (literal-vector-field 'Y S2-spherical))
              (define m ((point S2-spherical) (up 'theta 'phi)))
              (declare-argument-types! g (list vector-field? vector-field?))

              (check-equal?
               (simplify (((((covariant-derivative C) V) g) X Y) m))
               0))

   (test-case "ORIG:Cristoffel-symbols 1"
              (define-coordinates (up x y) R2-rect)
              (define R2-rect-basis
                (coordinate-system->basis R2-rect))
              (define R2-rect-point
                ((R2-rect '->point) (up 'x0 'y0)))
              (define (Gijk i j k)
                (literal-manifold-function
                 (string->symbol
                  (string-append "G^"
                                 (number->string i)
                                 "_"
                                 (number->string j)
                                 (number->string k)))
                 R2-rect))
              (define G
                (down (down (up (Gijk 0 0 0)
                                (Gijk 1 0 0))
                            (up (Gijk 0 1 0)
                                (Gijk 1 1 0)))
                      (down (up (Gijk 0 0 1)
                                (Gijk 1 0 1))
                            (up (Gijk 0 1 1)
                                (Gijk 1 1 1)))))
              (define CG (make-Christoffel G R2-rect-basis))
              (define CF (Christoffel->Cartan CG))

              ;(clear-arguments)
              ;(suppress-arguments '((up x0 y0))) <= this would remove the (up x0 y0) from the below result
              (check-equal?
               (simplify (G R2-rect-point))
               '(down
                  (down
                   (up (G^0_00 (up x0 y0)) (G^1_00 (up x0 y0)))
                   (up (G^0_10 (up x0 y0)) (G^1_10 (up x0 y0))))
                  (down
                   (up (G^0_01 (up x0 y0)) (G^1_01 (up x0 y0)))
                   (up (G^0_11 (up x0 y0)) (G^1_11 (up x0 y0))))))

              (check-equal?
               (simplify
                (((Cartan->forms CF) (literal-vector-field 'X R2-rect))
                 R2-rect-point))
               '(down
                 (up
                  (+
                   (* (G^0_00 (up x0 y0)) (X^0 (up x0 y0)))
                   (* (G^0_01 (up x0 y0)) (X^1 (up x0 y0))))
                  (+
                   (* (G^1_00 (up x0 y0)) (X^0 (up x0 y0)))
                   (* (G^1_01 (up x0 y0)) (X^1 (up x0 y0)))))
                 (up
                  (+
                   (* (G^0_10 (up x0 y0)) (X^0 (up x0 y0)))
                   (* (G^0_11 (up x0 y0)) (X^1 (up x0 y0))))
                  (+
                   (* (G^1_10 (up x0 y0)) (X^0 (up x0 y0)))
                   (* (G^1_11 (up x0 y0)) (X^1 (up x0 y0)))))))

              (check-equal?
               (simplify
                ((Christoffel->symbols
                  (Cartan->Christoffel (Christoffel->Cartan CG)))
                 R2-rect-point))
               '(down
                 (down
                  (up (G^0_00 (up x0 y0)) (G^1_00 (up x0 y0)))
                  (up (G^0_10 (up x0 y0)) (G^1_10 (up x0 y0))))
                 (down
                  (up (G^0_01 (up x0 y0)) (G^1_01 (up x0 y0)))
                  (up (G^0_11 (up x0 y0)) (G^1_11 (up x0 y0))))))

              (check-equal?
               (simplify
                (((((- (covariant-derivative CF)
                       (covariant-derivative
                        (Cartan-transform CF (R2-polar 'coordinate-basis))))
                    (literal-vector-field 'A R2-rect))
                   (literal-vector-field 'B R2-polar))
                  (literal-scalar-field 'f R2-polar))
                 R2-rect-point))
               0))

   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))