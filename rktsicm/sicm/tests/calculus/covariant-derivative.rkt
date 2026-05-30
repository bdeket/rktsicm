#lang racket/base

(require rackunit
         "../../main.rkt"
         "../helper+scm.rkt"
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

(provide the-tests)
(define the-tests
  (test-suite
   "calculus/covariant-derivative"
   (test-case
    "make-Christoffel"
    (define C (make-Christoffel 'symbols 'basis))
    (check-true (Christoffel? C))
    (check-equal? (Christoffel->symbols C) 'symbols)
    (check-equal? (Christoffel->basis C) 'basis))

   (test-case
    "make-Cartan"
    (define C (make-Cartan 'forms 'basis))
    (check-true (Cartan? C))
    (check-equal? (Cartan->forms C) 'forms)
    (check-equal? (Cartan->basis C) 'basis))

   (test-case
    "Cartan<->Christoffel"
    (define base (coordinate-system->basis R2-rect))
    (define pt ((point R2-rect) #(x0 y0)))
    (define ch (Cartan->Christoffel (make-Cartan (literal-1form-field 'f R2-rect) base)))
    (check-true (Christoffel? ch))
    (check-equal? (Christoffel->basis ch) base)
    (check-simplified? ((Christoffel->symbols ch) pt) '(down f_0 f_1))
    (check-simplified? ((Christoffel->symbols (Cartan->Christoffel (literal-Cartan 'C R2-rect))) pt)
                       '(down (down (up C_00^0 C_00^1) (up C_01^0 C_01^1))
                              (down (up C_10^0 C_10^1) (up C_11^0 C_11^1))))
    (check-simplified? (((Cartan->forms (literal-Cartan 'C R2-rect)) (literal-vector-field 'v R2-rect)) pt)
                       '(down (up (+ (* C_00^0 v^0) (* C_10^0 v^1)) (+ (* C_00^1 v^0) (* C_10^1 v^1)))
                              (up (+ (* C_01^0 v^0) (* C_11^0 v^1)) (+ (* C_01^1 v^0) (* C_11^1 v^1)))))

    (define ca (Christoffel->Cartan (make-Christoffel (down (down (up (literal-1form-field 'm0 R2-rect)
                                                                      (literal-1form-field 'm1 R2-rect))
                                                                  (up (literal-1form-field 'm2 R2-rect)
                                                                      (literal-1form-field 'm3 R2-rect)))
                                                            (down (up (literal-1form-field 'm4 R2-rect)
                                                                      (literal-1form-field 'm5 R2-rect))
                                                                  (up (literal-1form-field 'm6 R2-rect)
                                                                      (literal-1form-field 'm7 R2-rect))))
                                                      base)))
    (check-true (Cartan? ca))
    (check-equal? (Cartan->basis ca) base)
    (check-simplified? (((Cartan->forms ca) (literal-vector-field 'v0 R2-rect)
                                            (literal-vector-field 'v1 R2-rect)) pt)
                       '(down (up (+ (* -1 m0_1 v0^0 v1^1) (* m0_1 v0^1 v1^0) (* m4_0 v0^0 v1^1) (* -1 m4_0 v0^1 v1^0))
                                  (+ (* -1 m1_1 v0^0 v1^1) (* m1_1 v0^1 v1^0) (* m5_0 v0^0 v1^1) (* -1 m5_0 v0^1 v1^0)))
                              (up (+ (* -1 m2_1 v0^0 v1^1) (* m2_1 v0^1 v1^0) (* m6_0 v0^0 v1^1) (* -1 m6_0 v0^1 v1^0))
                                  (+ (* -1 m3_1 v0^0 v1^1) (* m3_1 v0^1 v1^0) (* m7_0 v0^0 v1^1) (* -1 m7_0 v0^1 v1^0)))))

    (check-exn #px"assertion failed: \\(Cartan\\? Cartan\\)"
               (λ () (Cartan->Christoffel 'not-a-cartan)))
    (check-exn #px"assertion failed: \\(Christoffel\\? Christoffel\\)"
               (λ () (Christoffel->Cartan 'not-a-christoffel))))


   (test-case
    "symmetrize-Cartan/Christoffel"
    (define pt ((point R2-rect) #(x0 y0)))
    (check-simplified? (((Cartan->forms (symmetrize-Cartan (literal-Cartan 'C R2-rect))) (literal-vector-field 'v R2-rect)) pt)
                       '(down (up (+ (* C_00^0 v^0) (* 1/2 C_01^0 v^1) (* 1/2 C_10^0 v^1))
                                  (+ (* C_00^1 v^0) (* 1/2 C_01^1 v^1) (* 1/2 C_10^1 v^1)))
                              (up (+ (* 1/2 C_01^0 v^0) (* 1/2 C_10^0 v^0) (* C_11^0 v^1))
                                  (+ (* 1/2 C_01^1 v^0) (* 1/2 C_10^1 v^0) (* C_11^1 v^1)))))

    (check-simplified? ((Christoffel->symbols (symmetrize-Christoffel (Cartan->Christoffel (literal-Cartan 'C R2-rect)))) pt)
                       '(down (down (up C_00^0 C_00^1)
                                    (up (+ (* 1/2 C_01^0) (* 1/2 C_10^0)) (+ (* 1/2 C_01^1) (* 1/2 C_10^1))))
                              (down (up (+ (* 1/2 C_01^0) (* 1/2 C_10^0)) (+ (* 1/2 C_01^1) (* 1/2 C_10^1)))
                                    (up C_11^0 C_11^1)))))
   (test-case
    "Cartan-transform"
    (define pt ((point R2-rect) #(x0 y0)))
    (check-simplified? (((Cartan->forms (Cartan-transform (literal-Cartan 'C R2-rect) (coordinate-system->basis R2-polar)))
                         (literal-vector-field 'v R2-rect)) pt)
                       (let* ([x 'x0] [y 'y0] [v0 'v^0] [v1 'v^1]
                              [x² (expt x 2)] [y² (expt y 2)] [x²+y² (+ x² y²)] [√x²+y² (sqrt x²+y²)] [xy (* x y)]
                              [CC0  (down 'C_00^0 'C_00^1 'C_01^0 'C_01^1)] [CC1  (down 'C_10^0 'C_10^1 'C_11^0 'C_11^1)]
                              [X1 (up x² xy xy y²)]         [CC_1 (+ (* v0 CC0 X1) (* v1 CC1 X1))]
                              [X2 (up (- xy) x² (- y²) xy)] [CC_2 (+ (* v0 CC0 X2) (* v1 CC1 X2))]
                              [X3 (up (- xy) (- y²) x² xy)] [CC_3 (+ (* v0 CC0 X3) (* v1 CC1 X3))]
                              [X4 (up y² (- xy) (- xy) x²)] [CC_4 (+ (* v0 CC0 X4) (* v1 CC1 X4))])
                         `(down (up (/ ,CC_1 ,x²+y²)
                                    (/ (+ ,CC_2 (* -1 v^0 y0) (* v^1 x0))
                                       (* ,√x²+y² (+ ,x² ,y²))))
                                (up (/ (+ ,CC_3 (* v^0 y0) (* -1 v^1 x0)) ,√x²+y²)
                                    (/ (+ ,CC_4 (* v^0 x0) (* v^1 y0)) ,x²+y²))))))

   (test-case
    "Cartan->Cartan-over-map"
    (define µ (literal-manifold-map 'µ R2-rect R3-rect))
    (define pt ((point R2-rect) #(x0 y0)))
    (check-simplified? (((Cartan->forms (Cartan->Cartan-over-map (literal-Cartan 'C R3-rect) µ))
                         ((vector-field->vector-field-over-map µ) (literal-vector-field 'v R3-rect)))
                        pt)
                       (let ([M '(up µ^0 µ^1 µ^2)])
                         `(down (up (+ (* (v^0 ,M) (C_00^0 ,M)) (* (v^1 ,M) (C_10^0 ,M)) (* (v^2 ,M) (C_20^0 ,M)))
                                    (+ (* (v^0 ,M) (C_00^1 ,M)) (* (v^1 ,M) (C_10^1 ,M)) (* (v^2 ,M) (C_20^1 ,M)))
                                    (+ (* (v^0 ,M) (C_00^2 ,M)) (* (v^1 ,M) (C_10^2 ,M)) (* (v^2 ,M) (C_20^2 ,M))))
                                (up (+ (* (v^0 ,M) (C_01^0 ,M)) (* (v^1 ,M) (C_11^0 ,M)) (* (v^2 ,M) (C_21^0 ,M)))
                                    (+ (* (v^0 ,M) (C_01^1 ,M)) (* (v^1 ,M) (C_11^1 ,M)) (* (v^2 ,M) (C_21^1 ,M)))
                                    (+ (* (v^0 ,M) (C_01^2 ,M)) (* (v^1 ,M) (C_11^2 ,M)) (* (v^2 ,M) (C_21^2 ,M))))
                                (up (+ (* (v^0 ,M) (C_02^0 ,M)) (* (v^1 ,M) (C_12^0 ,M)) (* (v^2 ,M) (C_22^0 ,M)))
                                    (+ (* (v^0 ,M) (C_02^1 ,M)) (* (v^1 ,M) (C_12^1 ,M)) (* (v^2 ,M) (C_22^1 ,M)))
                                    (+ (* (v^0 ,M) (C_02^2 ,M)) (* (v^1 ,M) (C_12^2 ,M)) (* (v^2 ,M) (C_22^2 ,M))))))))

   (test-case
    "covariant-derivative-function"
    (define pt ((point R2-rect) #(x0 y0)))
    ;; if only arg is a pt -> skips Cartan
    (check-simplified? ((((covariant-derivative-function 'not-a-cartan) (literal-1form-field 'f R2-rect))
                         (literal-vector-field 'v R2-rect)) pt)
                       '(+ (* f_0 v^0) (* f_1 v^1)))
    ;; if not 1-arg = pt, arguments can only be 1form- and vector-fields
    (check-exn #px"Bad function or arguments to covariant derivative"
               (λ () ((((covariant-derivative-function 'not-a-cartan) (literal-1form-field 'f R2-rect))
                       (literal-vector-field 'v R2-rect)) (literal-vector-field 'v R2-rect) pt)))
    (check-exn #px"Bad function or arguments to covariant derivative"
               (λ () ((((covariant-derivative-function 'not-a-cartan) (literal-1form-field 'f R2-rect))
                       (literal-vector-field 'v R2-rect)) (literal-1form-field 'v R2-rect) 3)))
    ;; otherwise type f and pass to cv-arg-types
    (define f (λ (x) (x (literal-manifold-function 'f R2-rect))))
    (define g (declare-argument-types! (λ (x) (x (literal-manifold-function 'f R2-rect)))
                                       (list vector-field?)))
    (define C (literal-Cartan 'C R2-rect))
    (define v1 (literal-vector-field 'v1 R2-rect))
    (define v2 (literal-vector-field 'v2 R2-rect))
    (check-equal? (((((covariant-derivative-function       C) v1) f) v2) pt)
                  (((((covariant-derivative-argument-types C) v1) g) v2) pt)))
   (test-case
    "covariant-derivative-vector"
    (define pt ((point R2-rect) #(x0 y0)))
    (define C (literal-Cartan 'C R2-rect))
    (define v1 (literal-vector-field 'v1 R2-rect))
    (define v2 (literal-vector-field 'v2 R2-rect))
    (define f (literal-manifold-function 'f R2-rect))
    (check-simplified? (((((covariant-derivative-vector C) v1) v2) f) pt)
                       '(+ (* C_00^0 v1^0 v2^0 ((partial 0) f))
                           (* C_00^1 v1^0 v2^0 ((partial 1) f))
                           (* C_01^0 v1^0 v2^1 ((partial 0) f))
                           (* C_01^1 v1^0 v2^1 ((partial 1) f))
                           (* C_10^0 v1^1 v2^0 ((partial 0) f))
                           (* C_10^1 v1^1 v2^0 ((partial 1) f))
                           (* C_11^0 v1^1 v2^1 ((partial 0) f))
                           (* C_11^1 v1^1 v2^1 ((partial 1) f))
                           (* v1^0 ((partial 0) f) ((partial 0) v2^0))
                           (* v1^0 ((partial 1) f) ((partial 0) v2^1))
                           (* v1^1 ((partial 0) f) ((partial 1) v2^0))
                           (* v1^1 ((partial 1) f) ((partial 1) v2^1)))))
   (test-case
    "covariant-derivative-form"
    (define pt ((point R2-rect) #(x0 y0)))
    (define C (literal-Cartan 'C R2-rect))
    (define v1 (literal-vector-field 'v1 R2-rect))
    (define 1f (literal-1form-field '1f R2-rect))
    (define v2 (literal-vector-field 'v2 R2-rect))
    (check-simplified? (((((covariant-derivative-form C) v1) 1f) v2) pt)
                       '(+ (* -1 1f_0 C_00^0 v1^0 v2^0)
                           (* -1 1f_0 C_01^0 v1^0 v2^1)
                           (* -1 1f_0 C_10^0 v1^1 v2^0)
                           (* -1 1f_0 C_11^0 v1^1 v2^1)
                           (* -1 1f_1 C_00^1 v1^0 v2^0)
                           (* -1 1f_1 C_01^1 v1^0 v2^1)
                           (* -1 1f_1 C_10^1 v1^1 v2^0)
                           (* -1 1f_1 C_11^1 v1^1 v2^1)
                           (* v1^0 v2^0 ((partial 0) 1f_0))
                           (* v1^0 v2^1 ((partial 0) 1f_1))
                           (* v1^1 v2^0 ((partial 1) 1f_0))
                           (* v1^1 v2^1 ((partial 1) 1f_1))))
    (check-exn #px"assertion failed: \\(= k \\(length vectors\\)\\)"
               (λ () (((((covariant-derivative-form C) v1) 1f) v2 v2) pt))))
   (test-case
    "covariant-derivative-argument-types"
    (define pt ((point R2-rect) #(x0 y0)))
    (define C (literal-Cartan 'C R2-rect))
    (define v1 (literal-vector-field 'v1 R2-rect))
    (define v2 (literal-vector-field 'v2 R2-rect))
    (define v3 (literal-vector-field 'v2 R2-rect))
    (define 1f (literal-1form-field '1f R2-rect))
    (check-equal? (argument-types (((covariant-derivative-argument-types C) v1)
                                   (declare-argument-types! (λ (x) x) (list vector-field? vector-field? 1form-field?))))
                  (list vector-field? vector-field? 1form-field?))
    (check-exn #px"assertion failed: \\(fix:= \\(length args\\) \\(length arg-types\\)\\)"
               (λ ()
                 ((((covariant-derivative-argument-types C) v1)
                   (declare-argument-types! (λ (x) x) (list vector-field? vector-field? 1form-field?)))
                  v2)))
    (check-exn #px"Bad arg types"
               (λ ()
                 ((((covariant-derivative-argument-types C) v1)
                   (declare-argument-types! (λ (x) x) (list vector-field? 1form-field? number?)))
                  v2 1f 3)))
    (check-exn #px"assertion failed: \\(vector-field\\? \\(car args\\)\\)"
               (λ ()
                 ((((covariant-derivative-argument-types C) v1)
                   (declare-argument-types! (λ (x) x) (list vector-field? 1form-field? number?)))
                  1f 1f 3)))
    (check-exn #px"assertion failed: \\(1form-field\\? \\(car args\\)\\)"
               (λ ()
                 ((((covariant-derivative-argument-types C) v1)
                   (declare-argument-types! (λ (x) x) (list vector-field? 1form-field? number?)))
                  v2 v2 3)))
    (check-simplified? (((((covariant-derivative-argument-types C) v1)
                          (declare-argument-types! (λ (v2 1f v3) (R2-rect '->coords)) (list vector-field? 1form-field? vector-field?)))
                         v2 1f v3) pt)
                       '(up (+
                             (* 1f_0 C_00^0 v1^0 x0)
                             (* 1f_0 C_01^0 v1^0 x0)
                             (* 1f_0 C_10^0 v1^1 x0)
                             (* 1f_0 C_11^0 v1^1 x0)
                             (* 1f_0 v1^0 (expt v2^0 2))
                             (* 2 1f_0 v1^0 v2^0 v2^1)
                             (* 1f_0 v1^0 (expt v2^1 2))
                             (* 1f_1 C_00^1 v1^0 x0)
                             (* 1f_1 C_01^1 v1^0 x0)
                             (* 1f_1 C_10^1 v1^1 x0)
                             (* 1f_1 C_11^1 v1^1 x0)
                             (* 1f_1 v1^0 (expt v2^0 2))
                             (* 2 1f_1 v1^0 v2^0 v2^1)
                             (* 1f_1 v1^0 (expt v2^1 2))
                             (* -2 C_00^0 v1^0 v2^0 x0)
                             (* -2 C_00^1 v1^0 v2^0 x0)
                             (* -2 C_01^0 v1^0 v2^1 x0)
                             (* -2 C_01^1 v1^0 v2^1 x0)
                             (* -2 C_10^0 v1^1 v2^0 x0)
                             (* -2 C_10^1 v1^1 v2^0 x0)
                             (* -2 C_11^0 v1^1 v2^1 x0)
                             (* -2 C_11^1 v1^1 v2^1 x0))
                            (+
                             (* 1f_0 C_00^0 v1^0 y0)
                             (* 1f_0 C_01^0 v1^0 y0)
                             (* 1f_0 C_10^0 v1^1 y0)
                             (* 1f_0 C_11^0 v1^1 y0)
                             (* 1f_0 v1^1 (expt v2^0 2))
                             (* 2 1f_0 v1^1 v2^0 v2^1)
                             (* 1f_0 v1^1 (expt v2^1 2))
                             (* 1f_1 C_00^1 v1^0 y0)
                             (* 1f_1 C_01^1 v1^0 y0)
                             (* 1f_1 C_10^1 v1^1 y0)
                             (* 1f_1 C_11^1 v1^1 y0)
                             (* 1f_1 v1^1 (expt v2^0 2))
                             (* 2 1f_1 v1^1 v2^0 v2^1)
                             (* 1f_1 v1^1 (expt v2^1 2))
                             (* -2 C_00^0 v1^0 v2^0 y0)
                             (* -2 C_00^1 v1^0 v2^0 y0)
                             (* -2 C_01^0 v1^0 v2^1 y0)
                             (* -2 C_01^1 v1^0 v2^1 y0)
                             (* -2 C_10^0 v1^1 v2^0 y0)
                             (* -2 C_10^1 v1^1 v2^0 y0)
                             (* -2 C_11^0 v1^1 v2^1 y0)
                             (* -2 C_11^1 v1^1 v2^1 y0)))))
   (test-case
    "covariant-derivative-ordinary"
    (define pt ((point R2-rect) #(x0 y0)))
    (define C (literal-Cartan 'C R2-rect))
    (define v1 (literal-vector-field 'v1 R2-rect))
    (define v2 (literal-vector-field 'v2 R2-rect))
    (define v3 (literal-vector-field 'v2 R2-rect))
    (define 1f (literal-1form-field '1f R2-rect))
    (define f (literal-manifold-function 'f R2-rect))
    (check-exn #px"assertion failed: \\(Cartan\\? Cartan\\)"
               (λ () (covariant-derivative-ordinary 'not-a-cartan)))
    (check-exn #px"Bad input -- covariant-derivative"
               (λ () (((covariant-derivative-ordinary C) 'any) 'incorrect-argument)))

    (check-equal? (((((covariant-derivative-vector   C) v1) v2) f) pt)
                  (((((covariant-derivative-ordinary C) v1) v2) f) pt))
    (check-equal? (((((covariant-derivative-form     C) v1) 1f) v2) pt)
                  (((((covariant-derivative-ordinary C) v1) 1f) v2) pt))
    (define F (declare-argument-types! (λ (v2 1f v3) (R2-rect '->coords)) (list vector-field? 1form-field? vector-field?)))
    (check-equal? (((((covariant-derivative-argument-types C) v1) F) v2 1f v3) pt)
                  (((((covariant-derivative-ordinary       C) v1) F) v2 1f v3) pt))
    (define G (λ (v2 1f v3) (R2-rect '->coords)))
    (check-equal? (((((covariant-derivative-function C) v1) G) v2 1f v3) pt)
                  (((((covariant-derivative-ordinary C) v1) G) v2 1f v3) pt))
    (check-simplified? ((((covariant-derivative-ordinary C) v1) (up (literal-manifold-function 'f R2-rect))) pt)
                       '(up (+ (* v1^0 ((partial 0) f)) (* v1^1 ((partial 1) f))))))
   (test-case
    "covariant-derivative / differential"
    (define pt ((point R2-rect) #(x0 y0)))
    (define C (literal-Cartan 'C R2-rect))
    (define v1 (literal-vector-field 'v1 R2-rect))
    (define v2 (literal-vector-field 'v2 R2-rect))
    (define f (literal-manifold-function 'f R2-rect))
    (check-simplified? (((((covariant-derivative-vector   C) v1) v2) f) pt)
                       (((((covariant-derivative C) v1) v2) f) pt))
    (check-simplified? (((((covariant-derivative-vector   C) v1) v2) f) pt)
                       (((((covariant-differential C) v2) v1) f) pt)))
   (test-case
    "covariant-derivative + map"
    ;; TODO: m != n
    (define M R2-rect) (define N R2-rect)
    (define pt ((point N) #(x0 y0)))
    (define C (literal-Cartan 'C M))
    (define µ (literal-manifold-map 'µ N M))
    (define µvf (vector-field->vector-field-over-map µ))
    (define v1 (µvf (literal-vector-field 'v1 M)))
    (define v2 (µvf (literal-vector-field 'v2 M)))
    (define 1f ((form-field->form-field-over-map µ) (literal-1form-field '1f M)))
    (check-simplified? (((((covariant-derivative C µ) v1) 1f) v2) pt)
                       (let* ([M '(up µ^0 µ^1)] [M^1 `(up (µ^0 ,M) (µ^1 ,M))])
                         `(+ (* -1 (v1^0 ,M) (((partial 0) µ^1) ,M) (1f_0 ,M) (v2^0 ,M) (C_10^0 ,M))
                             (* -1 (v1^0 ,M) (((partial 0) µ^1) ,M) (1f_0 ,M) (v2^1 ,M) (C_11^0 ,M))
                             (* -1 (v1^0 ,M) (((partial 0) µ^1) ,M) (v2^0 ,M) (1f_1 ,M) (C_10^1 ,M))
                             (* -1 (v1^0 ,M) (((partial 0) µ^1) ,M) (v2^1 ,M) (1f_1 ,M) (C_11^1 ,M))
                             (* -1 (v1^0 ,M) (1f_0 ,M) (v2^0 ,M) (((partial 0) µ^0) ,M) (C_00^0 ,M))
                             (* -1 (v1^0 ,M) (1f_0 ,M) (v2^1 ,M) (((partial 0) µ^0) ,M) (C_01^0 ,M))
                             (* -1 (v1^0 ,M) (v2^0 ,M) (1f_1 ,M) (((partial 0) µ^0) ,M) (C_00^1 ,M))
                             (* -1 (v1^0 ,M) (v2^1 ,M) (1f_1 ,M) (((partial 0) µ^0) ,M) (C_01^1 ,M))
                             (* -1 (1f_0 ,M) (v2^0 ,M) (C_10^0 ,M) (v1^1 ,M) (((partial 1) µ^1) ,M))
                             (* -1 (1f_0 ,M) (v2^0 ,M) (C_00^0 ,M) (v1^1 ,M) (((partial 1) µ^0) ,M))
                             (* -1 (1f_0 ,M) (v2^1 ,M) (C_11^0 ,M) (v1^1 ,M) (((partial 1) µ^1) ,M))
                             (* -1 (1f_0 ,M) (v2^1 ,M) (C_01^0 ,M) (v1^1 ,M) (((partial 1) µ^0) ,M))
                             (* -1 (v2^0 ,M) (1f_1 ,M) (C_10^1 ,M) (v1^1 ,M) (((partial 1) µ^1) ,M))
                             (* -1 (v2^0 ,M) (1f_1 ,M) (C_00^1 ,M) (v1^1 ,M) (((partial 1) µ^0) ,M))
                             (* -1 (v2^1 ,M) (1f_1 ,M) (C_11^1 ,M) (v1^1 ,M) (((partial 1) µ^1) ,M))
                             (* -1 (v2^1 ,M) (1f_1 ,M) (C_01^1 ,M) (v1^1 ,M) (((partial 1) µ^0) ,M))
                             (* -1 (v1^0 ,M) (((partial 0) µ^1) ,M) (1f_0 ,M) (((partial 1) v2^0) ,M^1))
                             (* -1 (v1^0 ,M) (((partial 0) µ^1) ,M) (1f_1 ,M) (((partial 1) v2^1) ,M^1))
                             (* (v1^0 ,M) (((partial 0) µ^1) ,M) (v2^0 ,M^1) (((partial 1) 1f_0) ,M^1))
                             (* (v1^0 ,M) (((partial 0) µ^1) ,M) (((partial 1) v2^0) ,M^1) (1f_0 ,M^1))
                             (* (v1^0 ,M) (((partial 0) µ^1) ,M) (v2^1 ,M^1) (((partial 1) 1f_1) ,M^1))
                             (* (v1^0 ,M) (((partial 0) µ^1) ,M) (((partial 1) v2^1) ,M^1) (1f_1 ,M^1))
                             (* -1 (v1^0 ,M) (1f_0 ,M) (((partial 0) µ^0) ,M) (((partial 0) v2^0) ,M^1))
                             (* -1 (v1^0 ,M) (1f_1 ,M) (((partial 0) µ^0) ,M) (((partial 0) v2^1) ,M^1))
                             (* (v1^0 ,M) (((partial 0) µ^0) ,M) (v2^0 ,M^1) (((partial 0) 1f_0) ,M^1))
                             (* (v1^0 ,M) (((partial 0) µ^0) ,M) (1f_0 ,M^1) (((partial 0) v2^0) ,M^1))
                             (* (v1^0 ,M) (((partial 0) µ^0) ,M) (v2^1 ,M^1) (((partial 0) 1f_1) ,M^1))
                             (* (v1^0 ,M) (((partial 0) µ^0) ,M) (1f_1 ,M^1) (((partial 0) v2^1) ,M^1))
                             (* -1 (1f_0 ,M) (v1^1 ,M) (((partial 1) µ^1) ,M) (((partial 1) v2^0) ,M^1))
                             (* -1 (1f_0 ,M) (v1^1 ,M) (((partial 1) µ^0) ,M) (((partial 0) v2^0) ,M^1))
                             (* -1 (1f_1 ,M) (v1^1 ,M) (((partial 1) µ^1) ,M) (((partial 1) v2^1) ,M^1))
                             (* -1 (1f_1 ,M) (v1^1 ,M) (((partial 1) µ^0) ,M) (((partial 0) v2^1) ,M^1))
                             (* (v1^1 ,M) (((partial 1) µ^1) ,M) (v2^0 ,M^1) (((partial 1) 1f_0) ,M^1))
                             (* (v1^1 ,M) (((partial 1) µ^1) ,M) (((partial 1) v2^0) ,M^1) (1f_0 ,M^1))
                             (* (v1^1 ,M) (((partial 1) µ^1) ,M) (v2^1 ,M^1) (((partial 1) 1f_1) ,M^1))
                             (* (v1^1 ,M) (((partial 1) µ^1) ,M) (((partial 1) v2^1) ,M^1) (1f_1 ,M^1))
                             (* (v1^1 ,M) (((partial 1) µ^0) ,M) (v2^0 ,M^1) (((partial 0) 1f_0) ,M^1))
                             (* (v1^1 ,M) (((partial 1) µ^0) ,M) (1f_0 ,M^1) (((partial 0) v2^0) ,M^1))
                             (* (v1^1 ,M) (((partial 1) µ^0) ,M) (v2^1 ,M^1) (((partial 0) 1f_1) ,M^1))
                             (* (v1^1 ,M) (((partial 1) µ^0) ,M) (1f_1 ,M^1) (((partial 0) v2^1) ,M^1))))))

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
              (define R2-rect-basis (coordinate-system->basis R2-rect))
              (define R2-rect-point ((R2-rect '->point) (up 'x0 'y0)))
              (define-values (G CG CF) (make-GCF R2-rect-basis))

              (check-simplified? (G R2-rect-point)
                                 '(down
                                   (down (up G^0_00 G^1_00) (up G^0_10 G^1_10))
                                   (down (up G^0_01 G^1_01) (up G^0_11 G^1_11))))

              (check-simplified? (((Cartan->forms CF) (literal-vector-field 'X R2-rect))
                                  R2-rect-point)
                                 '(down
                                   (up
                                    (+ (* G^0_00 X^0) (* G^0_01 X^1))
                                    (+ (* G^1_00 X^0) (* G^1_01 X^1)))
                                   (up
                                    (+ (* G^0_10 X^0) (* G^0_11 X^1))
                                    (+ (* G^1_10 X^0) (* G^1_11 X^1)))))

              (check-simplified? ((Christoffel->symbols
                                   (Cartan->Christoffel (Christoffel->Cartan CG)))
                                  R2-rect-point)
                                 '(down
                                   (down (up G^0_00 G^1_00) (up G^0_10 G^1_10))
                                   (down (up G^0_01 G^1_01) (up G^0_11 G^1_11))))

              (check-simplified? (((((- (covariant-derivative CF)
                                        (covariant-derivative
                                         (Cartan-transform CF (R2-polar 'coordinate-basis))))
                                     (literal-vector-field 'A R2-rect))
                                    (literal-vector-field 'B R2-polar))
                                   (literal-scalar-field 'f R2-polar))
                                  R2-rect-point)
                                 0))
   (test-case
    "example from text"
    (define-coordinates (up x y) R2-rect)
    (define-coordinates (up r theta) R2-polar)
    (define R2-rect-point ((R2-rect '->point) (up 'x0 'y0)))
    (define v (literal-vector-field 'v R2-rect))
    (define w (literal-vector-field 'w R2-rect))
    (define f (literal-manifold-function 'f R2-rect))
    (define R2-rect-basis (coordinate-system->basis R2-rect))
    (define R2-polar-basis (coordinate-system->basis R2-polar))
    (define R2-rect-Christoffel
      (make-Christoffel
       (let ((zero (lambda (m) 0)))
         (down (down (up zero zero)
                     (up zero zero))
               (down (up zero zero)
                     (up zero zero))))
       R2-rect-basis))
    (define R2-rect-Cartan (Christoffel->Cartan R2-rect-Christoffel))
    (define R2-polar-Christoffel
      (make-Christoffel
       (let ((zero (lambda (m) 0)))
         (down (down (up zero zero)
                     (up zero (/ 1 r)))
               (down (up zero (/ 1 r))
                     (up (* -1 r) zero))))
       R2-polar-basis))
    (define R2-polar-Cartan (Christoffel->Cartan R2-polar-Christoffel))
    (define-values (G CG CF) (make-GCF R2-rect-basis))
    (check-simplified? (((((- (covariant-derivative R2-rect-Cartan)
                              (covariant-derivative R2-polar-Cartan))
                           v)
                          w)
                         f)
                        (typical-point R2-rect))
                       0)
    (check-simplified? (((((- (covariant-derivative R2-polar-Cartan)
                              (covariant-derivative
                               (Cartan-transform R2-polar-Cartan R2-rect-basis)))
                           v)
                          w)
                         f)
                        R2-rect-point)
                       0)

    (define X (literal-vector-field 'X R2-rect))
    (define V (literal-vector-field 'V R2-rect))
    (check-simplified? (((((covariant-derivative CF) X) V)
                         (literal-manifold-function 'F R2-rect))
                        R2-rect-point)
                       '(+ (* G^0_00 V^0 ((partial 0) F) X^0)
                           (* G^1_00 V^0 ((partial 1) F) X^0)
                           (* G^0_10 ((partial 0) F) V^1 X^0)
                           (* G^1_10 ((partial 1) F) V^1 X^0)
                           (* G^0_01 V^0 ((partial 0) F) X^1)
                           (* G^1_01 V^0 ((partial 1) F) X^1)
                           (* G^0_11 ((partial 0) F) V^1 X^1)
                           (* G^1_11 ((partial 1) F) V^1 X^1)
                           (* ((partial 0) F) ((partial 0) V^0) X^0)
                           (* ((partial 0) F) ((partial 1) V^0) X^1)
                           (* ((partial 1) F) ((partial 0) V^1) X^0)
                           (* ((partial 1) F) ((partial 1) V^1) X^1))))
   (test-case
    "rect/polar"
    (define-coordinates (up x y) R2-rect)
    (define rect-basis (coordinate-system->basis R2-rect))
    (define-coordinates (up r theta) R2-polar)
    (define polar-basis (coordinate-system->basis R2-polar))
    (define rect-chi (R2-rect '->coords))
    (define rect-chi-inverse (R2-rect '->point))
    (define polar-chi (R2-polar '->coords))
    (define polar-chi-inverse (R2-polar '->point))
    (define m2 (rect-chi-inverse (up 'x0 'y0)))
    (define rect-Christoffel
      (make-Christoffel
       (let ((zero (lambda (m) 0)))
         (down (down (up zero zero)
                     (up zero zero))
               (down (up zero zero)
                     (up zero zero))))
       rect-basis))
    (define polar-Christoffel
      (make-Christoffel
       (let ((zero (lambda (m) 0)))
         (down (down (up zero zero)
                     (up zero (/ 1 r)))
               (down (up zero (/ 1 r))
                     (up (* -1 r) zero))))
       polar-basis))
    (define rect-Cartan (Christoffel->Cartan rect-Christoffel))
    (define polar-Cartan (Christoffel->Cartan polar-Christoffel))
    (define J (- (* x d/dy) (* y d/dx)))
    (define f (literal-scalar-field 'f R2-rect))
    ;;; Note: arg-suppressor is in force from above.
    (check-simplified? (((((covariant-derivative rect-Cartan)
                           d/dx)
                          J)
                         f)
                        m2)
                       '((partial 1) f))
    (check-simplified? (((((covariant-derivative polar-Cartan)
                           d/dx)
                          J)
                         f)
                        m2)
                       '((partial 1) f))
    ;independence - rect
    (define v (literal-vector-field 'v R2-rect))
    (define w (literal-vector-field 'w R2-rect))
    (check-simplified? (((((- (covariant-derivative rect-Cartan)
                              (covariant-derivative polar-Cartan))
                           v)
                          w)
                         f)
                        m2)
                       0)
    ;independence - polar
    (define vp (literal-vector-field 'v R2-polar))
    (define wp (literal-vector-field 'w R2-polar))
    (check-simplified? (((((- (covariant-derivative rect-Cartan)
                              (covariant-derivative polar-Cartan))
                           vp)
                          wp)
                         f)
                        m2)
                       0))
   (test-case
    "S^2"
    (define M (make-manifold S^2-type 2 3))
    (define spherical (coordinate-system-at 'spherical 'north-pole M))
    (define-coordinates (up theta phi) spherical)
    (define-coordinates t the-real-line)
    (define spherical-basis (coordinate-system->basis spherical))
    (define G-S2-1
      (make-Christoffel
       (let ((zero  (lambda (point) 0)))
         (down (down (up zero zero)
                     (up zero (/ 1 (tan theta))))
               (down (up zero (/ 1 (tan theta)))
                     (up (- (* (sin theta) (cos theta))) zero))))
       spherical-basis))
    (define gamma:N->M
      (compose (spherical '->point)
               (up (literal-function 'alpha)
                   (literal-function 'beta))
               (the-real-line '->coords)))
    (define basis-over-gamma (basis->basis-over-map gamma:N->M spherical-basis))
    (define w
      (basis-components->vector-field
       (up (compose (literal-function 'w0)
                    (the-real-line '->coords))
           (compose (literal-function 'w1)
                    (the-real-line '->coords)))
       (basis->vector-basis basis-over-gamma)))
    (define sphere-Cartan (Christoffel->Cartan G-S2-1))
    (check-simplified? (s:map/r
                        (lambda (omega)
                          ((omega
                            (((covariant-derivative sphere-Cartan gamma:N->M)
                              d/dt)
                             w))
                           ((the-real-line '->point) 'tau)))
                        (basis->1form-basis basis-over-gamma))
                       `(up
                         (+ (* -1 (sin (alpha tau)) ((D beta) tau) (w1 tau) (cos (alpha tau)))
                            ((D w0) tau))
                         (+ (/ (* (w0 tau) ((D beta) tau) (cos (alpha tau))) (sin (alpha tau)))
                            (/ (* (w1 tau) ((D alpha) tau) (cos (alpha tau))) (sin (alpha tau)))
                            ((D w1) tau))))
    (check-simplified? (s:map/r
                        (lambda (omega)
                          ((omega
                            (((covariant-derivative sphere-Cartan gamma:N->M)
                              d/dt)
                             ((differential gamma:N->M) d/dt)))
                           ((the-real-line '->point) 't)))
                        (basis->1form-basis basis-over-gamma))
                       `(up
                         (+ (* -1 (sin (alpha t)) (expt ((D beta) t) 2) (cos (alpha t)))
                            (((expt D 2) alpha) t))
                         (+ (/ (* 2 ((D beta) t) (cos (alpha t)) ((D alpha) t)) (sin (alpha t)))
                            (((expt D 2) beta) t)))))
   (test-case
    "geodesic"
    (define-coordinates (up x y) R2-rect)
    (define-coordinates t the-real-line)
    (define-values (G CG CF) (make-GCF (coordinate-system->basis R2-rect)))
    (define gamma:N->M
      (compose (R2-rect '->point)
               (up (literal-function 'alpha)
                   (literal-function 'beta))
               (the-real-line '->coords)))
    (define basis-over-gamma
      (basis->basis-over-map gamma:N->M
                             (coordinate-system->basis R2-rect)))
    (define u
      (basis-components->vector-field
       (up (compose (literal-function 'u0)
                    (the-real-line '->coords))
           (compose (literal-function 'u1)
                    (the-real-line '->coords)))
       (basis->vector-basis basis-over-gamma)))
    (check-simplified? (s:map/r
                        (lambda (omega)
                          ((omega
                            (((covariant-derivative (Christoffel->Cartan CG) gamma:N->M)
                              d/dt)
                             u))
                           ((the-real-line '->point) 't)))
                        (basis->1form-basis basis-over-gamma))
                       `(up
                         (+ (* ((D alpha) t) (u0 t) (G^0_00 (up (alpha t) (beta t))))
                            (* ((D alpha) t) (u1 t) (G^0_10 (up (alpha t) (beta t))))
                            (* ((D beta) t) (u0 t) (G^0_01 (up (alpha t) (beta t))))
                            (* ((D beta) t) (u1 t) (G^0_11 (up (alpha t) (beta t))))
                            ((D u0) t))
                         (+ (* ((D alpha) t) (u0 t) (G^1_00 (up (alpha t) (beta t))))
                            (* ((D alpha) t) (u1 t) (G^1_10 (up (alpha t) (beta t))))
                            (* ((D beta) t) (u0 t) (G^1_01 (up (alpha t) (beta t))))
                            (* ((D beta) t) (u1 t) (G^1_11 (up (alpha t) (beta t))))
                            ((D u1) t))))
    (check-simplified? (s:map/r
                        (lambda (omega)
                          ((omega
                            (((covariant-derivative (Christoffel->Cartan CG) gamma:N->M)
                              d/dt)
                             ((differential gamma:N->M) d/dt)))
                           ((the-real-line '->point) 't)))
                        (basis->1form-basis basis-over-gamma))
                       `(up
                         (+ (* (expt ((D alpha) t) 2) (G^0_00 (up (alpha t) (beta t))))
                            (* ((D alpha) t) ((D beta) t) (G^0_01 (up (alpha t) (beta t))))
                            (* ((D alpha) t) ((D beta) t) (G^0_10 (up (alpha t) (beta t))))
                            (* (expt ((D beta) t) 2) (G^0_11 (up (alpha t) (beta t))))
                            (((expt D 2) alpha) t))
                         (+ (* (expt ((D alpha) t) 2) (G^1_00 (up (alpha t) (beta t))))
                            (* ((D alpha) t) ((D beta) t) (G^1_01 (up (alpha t) (beta t))))
                            (* ((D alpha) t) ((D beta) t) (G^1_10 (up (alpha t) (beta t))))
                            (* (expt ((D beta) t) 2) (G^1_11 (up (alpha t) (beta t))))
                            (((expt D 2) beta) t)))))
   (test-case
    "Geodesic equations = Lagrange equations"
    (define-coordinates t R1-rect)
    (define-coordinates (up theta phi) S2-spherical)
    (define 2-sphere-basis (coordinate-system->basis S2-spherical))
    (define G-S2-1
      (make-Christoffel
       (let ((zero  (lambda (point) 0)))
         (down (down (up zero zero)
                     (up zero (/ 1 (tan theta))))
               (down (up zero (/ 1 (tan theta)))
                     (up (- (* (sin theta) (cos theta))) zero))))
       2-sphere-basis))
    (check-simplified? (let ((mu:N->M (compose (S2-spherical '->point)
                                               (up (literal-function 'mu-theta)
                                                   (literal-function 'mu-phi))
                                               (R1-rect '->coords)))
                             (Cartan (Christoffel->Cartan G-S2-1)))
                         (s:map/r
                          (lambda (w)
                            ((w
                              (((covariant-derivative Cartan mu:N->M) d/dt)
                               ((differential mu:N->M) d/dt)))
                             ((R1-rect '->point) 'tau)))
                          (basis->1form-basis
                           (basis->basis-over-map mu:N->M
                                                  (Cartan->basis Cartan)))))
                       `(up (+ (* -1
                                  (expt ((D mu-phi) tau) 2)
                                  (cos (mu-theta tau))
                                  (sin (mu-theta tau)))
                               (((expt D 2) mu-theta) tau))
                            (+ (/ (* 2 ((D mu-phi) tau)
                                     (cos (mu-theta tau))
                                     ((D mu-theta) tau))
                                  (sin (mu-theta tau)))
                               (((expt D 2) mu-phi) tau))))

    (define ((Lfree m) s)
      (let ((t (time s))
            (q (coordinate s))
            (v (velocity s)))
        (* 1/2 m (square v))))
    (define F
      (compose (R3-rect '->coords)
               (transfer-point S2-spherical R3-rect)
               (S2-spherical '->point)
               coordinate))
    (define Lsphere
      (compose (Lfree 1) (F->C F)))
    (check-simplified? (((Lagrange-equations Lsphere)
                         (up (literal-function 'theta)
                             (literal-function 'phi)))
                        't)
                       '(down
                         (+ (((expt D 2) theta) t)
                            (* -1 (cos (theta t)) (sin (theta t)) (expt ((D phi) t) 2)))
                         (+ (* (expt (sin (theta t)) 2) (((expt D 2) phi) t))
                            (* 2 (cos (theta t)) (sin (theta t)) ((D phi) t) ((D theta) t))))))
   (test-case
    "Exercise on computation of Christoffel symbols"
    (define-coordinates (up x y z) R3-rect)
    (define R3-rect-point ((R3-rect '->point) (up 'x0 'y0 'z0)))
    (define-coordinates (up r theta zeta) R3-cyl)
    (define R3-cyl-point ((R3-cyl '->point) (up 'r0 'theta0 'z0)))
    (define mpr (R3-rect '->coords))
    (check-simplified? (((* d/dr d/dr) mpr) R3-rect-point)
                       (up 0 0 0))
    (check-simplified? (((* d/dtheta d/dr) mpr) R3-rect-point)
                       '(up (/ (* -1 y0) (sqrt (+ (expt x0 2) (expt y0 2))))
                            (/ x0 (sqrt (+ (expt x0 2) (expt y0 2))))
                            0))
    (check-simplified? (((* d/dtheta d/dr) mpr) R3-cyl-point)
                       '(up (* -1 (sin theta0)) (cos theta0) 0))
    (check-simplified? (((* d/dr d/dtheta) mpr) R3-rect-point)
                       '(up (/ (* -1 y0) (sqrt (+ (expt x0 2) (expt y0 2))))
                            (/ x0 (sqrt (+ (expt x0 2) (expt y0 2))))
                            0))
    (check-simplified? (((* d/dr d/dtheta) mpr) R3-cyl-point)
                       '(up (* -1 (sin theta0)) (cos theta0) 0))
    (check-simplified? (((* d/dtheta d/dtheta) mpr) R3-rect-point)
                       '(up (* -1 x0) (* -1 y0) 0))
    (check-simplified? (((* d/dtheta d/dtheta) mpr) R3-cyl-point)
                       '(up (* -1 r0 (cos theta0)) (* -1 r0 (sin theta0)) 0)))
   (test-case
    "Computation of Covariant derivatives by difference quotient"
    (define-values (G CG CF) (make-GCF (coordinate-system->basis R2-rect)))
    (define X (literal-vector-field 'X R2-rect))
    (define Y (literal-vector-field 'Y R2-rect))
    (define q_0 (up 'q_x 'q_y))
    (define m_0 ((R2-rect '->point) q_0))
    (define F (literal-manifold-function 'F R2-rect))
    (define (((((CD CF chart) v) u) F) m) ;; simple version

      (define (Sigma state) (ref state 0))
      (define (U state) (ref state 1))
      (define (sigma-u sigma u) (up sigma u))

      (define chi (chart '->coords))
      (define chi^-1 (chart '->point))

      ;; ((gamma m) delta) is the point on gamma advanced by delta.

      (define ((gamma m) delta)
        (chi^-1 (+ (chi m) (* delta ((v chi) m)))))

      (let ((basis (Cartan->basis CF)))
        (let ((vector-basis (basis->vector-basis basis))
              (1form-basis (basis->1form-basis basis)))
          (let ((u^i (1form-basis u)))
            (let ((initial-state
                   (sigma-u (chi m) (u^i m))))

              ;; First-order approximation to A

              (define (Au delta)
                (- (u^i m)
                   (* delta
                      (((Cartan->forms CF) v) m)
                      (u^i m))))

              (define (g delta)
                (let ((advanced-m ((gamma m) delta)))
                  (* (- (u^i advanced-m) (Au delta))
                     ((vector-basis F) advanced-m))))

              ((D g) 0))))))
    (check-simplified? (let ((CF (Christoffel->Cartan
                                  (make-Christoffel G
                                                    (coordinate-system->basis R2-rect)))))
                         (- (((((CD CF R2-rect) X) Y) F) m_0)
                            (((((covariant-derivative CF) X) Y) F) m_0)))
                       0)
    (check-simplified? (let ((CF (Christoffel->Cartan
                                  (make-Christoffel G
                                                    (coordinate-system->basis R2-polar)))))
                         (- (((((CD CF R2-rect) X) Y) F) m_0)
                            (((((covariant-derivative CF) X) Y) F) m_0)))
                       0)
    (check-simplified? (let ((CF (Christoffel->Cartan
                                  (make-Christoffel G
                                                    (coordinate-system->basis R2-rect)))))
                         (- (((((CD CF R2-polar) X) Y) F) m_0)
                            (((((covariant-derivative CF) X) Y) F) m_0)))
                       0)
    (check-simplified? (let ((CF (Christoffel->Cartan
                                  (make-Christoffel G
                                                    (coordinate-system->basis R2-polar)))))
                         (- (((((CD CF R2-polar) X) Y) F) m_0)
                            (((((covariant-derivative CF) X) Y) F) m_0)))
                       0))
   (test-case
    "Testing on forms"
    (define-values (G CG CF) (make-GCF (coordinate-system->basis R2-rect)))
    (define X (literal-vector-field 'X R2-rect))
    (define Y (literal-vector-field 'Y R2-rect))
    (define omega (literal-1form-field 'omega R2-rect))
    (define q_0 (up 'q_x 'q_y))
    (define m_0 ((R2-rect '->point) q_0))
    (define F (literal-manifold-function 'F R2-rect))
    (check-simplified? (let* ((CF (Christoffel->Cartan
                                   (make-Christoffel G
                                                     (coordinate-system->basis R2-rect))))
                              (D_x ((covariant-derivative CF) X)))
                         (- (+ (((D_x omega) Y) m_0)
                               ((omega (D_x Y)) m_0))
                            ((D_x (omega Y)) m_0)))
                       0)

    (define tau (literal-1form-field 'tau R2-rect))
    (define Z (literal-vector-field 'Z R2-rect))
    (check-simplified? (let* ((CF (Christoffel->Cartan
                                   (make-Christoffel G
                                                     (coordinate-system->basis R2-rect))))
                              (D_x ((covariant-derivative CF) X)))
                         (- (((D_x (wedge omega tau)) Y Z) m_0)
                            (+ (((wedge omega (D_x tau)) Y Z) m_0)
                               (((wedge (D_x omega) tau) Y Z) m_0))))
                       0)
    (check-simplified? (let* ((CF (Christoffel->Cartan
                                   (make-Christoffel G
                                                     (coordinate-system->basis R2-polar))))
                              (D_x ((covariant-derivative CF) X)))
                         (- (((D_x (wedge omega tau)) Y Z) m_0)
                            (+ (((wedge omega (D_x tau)) Y Z) m_0)
                               (((wedge (D_x omega) tau) Y Z) m_0))))
                       0))
   (test-case
    "geodesic-equation1"
    (check-simplified? (((geodesic-equation the-real-line R2-rect (literal-Cartan 'G R2-rect))
                         (literal-manifold-map 'gamma the-real-line R2-rect))
                        ((point the-real-line) 't))
                       '(up
                         (+ (* (expt ((D gamma^0) t) 2) (G_00^0 (up (gamma^0 t) (gamma^1 t))))
                            (* ((D gamma^0) t) ((D gamma^1) t) (G_10^0 (up (gamma^0 t) (gamma^1 t))))
                            (* ((D gamma^0) t) ((D gamma^1) t) (G_01^0 (up (gamma^0 t) (gamma^1 t))))
                            (* (expt ((D gamma^1) t) 2) (G_11^0 (up (gamma^0 t) (gamma^1 t))))
                            (((expt D 2) gamma^0) t))
                         (+ (* (expt ((D gamma^0) t) 2) (G_00^1 (up (gamma^0 t) (gamma^1 t))))
                            (* ((D gamma^0) t) ((D gamma^1) t) (G_10^1 (up (gamma^0 t) (gamma^1 t))))
                            (* ((D gamma^0) t) ((D gamma^1) t) (G_01^1 (up (gamma^0 t) (gamma^1 t))))
                            (* (expt ((D gamma^1) t) 2) (G_11^1 (up (gamma^0 t) (gamma^1 t))))
                            (((expt D 2) gamma^1) t)))))
   (test-case
    "geodesic-equation2"
    (check-simplified? (let ((C (literal-Cartan 'G R2-rect)))
                         (- (((geodesic-equation the-real-line R2-rect C)
                              (literal-manifold-map 'gamma the-real-line R2-rect))
                             ((point the-real-line) 't))
                            (((geodesic-equation the-real-line R2-rect (symmetrize-Cartan C))
                              (literal-manifold-map 'gamma the-real-line R2-rect))
                             ((point the-real-line) 't))))
                       (up 0 0)))
   (test-case
    "Manifold S^2"
    (define M (make-manifold S^2-type 2 3))
    (define S2-spherical (coordinate-system-at 'spherical 'north-pole M))
    (define-coordinates (up theta phi) S2-spherical)
    (define S2-basis (coordinate-system->basis S2-spherical))
    (define G-S2-1
      (make-Christoffel
       (let ((zero  (lambda (point) 0)))
         (down (down (up zero zero)
                     (up zero (/ 1 (tan theta))))
               (down (up zero (/ 1 (tan theta)))
                     (up (- (* (sin theta)
                               (cos theta)))
                         zero))))
       S2-basis))
    (define gamma
      (compose (point S2-spherical)
               (up (literal-function 'alpha)
                   (literal-function 'beta))
               (chart the-real-line)))
    (define basis-over-gamma (basis->basis-over-map gamma S2-basis))
    (define u
      (basis-components->vector-field
       (up (compose (literal-function 'u^0)
                    (chart the-real-line))
           (compose (literal-function 'u^1)
                    (chart the-real-line)))
       (basis->vector-basis basis-over-gamma)))
    (define sphere-Cartan (Christoffel->Cartan G-S2-1))
    (check-simplified? ((((parallel-transport-equation
                           the-real-line S2-spherical sphere-Cartan)
                          gamma)
                         u)
                        ((point the-real-line) 't))
                       '(up
                         (+ (* -1 (sin (alpha t)) ((D beta) t) (u^1 t) (cos (alpha t))) ((D u^0) t))
                         (+ (/ (* (u^0 t) ((D beta) t) (cos (alpha t))) (sin (alpha t)))
                            (/ (* (u^1 t) ((D alpha) t) (cos (alpha t))) (sin (alpha t)))
                            ((D u^1) t)))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))