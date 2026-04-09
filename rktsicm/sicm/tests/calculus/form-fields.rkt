#lang racket/base

(require rackunit
         "../../main.rkt"
         "../../general/eq-properties.rkt"
         "../helper+scm.rkt"
         )

(provide the-tests)

(define (check-ff nf [name 'unnamed-1form-field]
                  #:rank [rank 1]
                  #:fields [fields (build-list rank (λ (_) vector-field?))])
  (check-true (form-field? nf))
  (when (= rank 1) (1form-field? nf))
  (check-true (operator? nf))
  (check-equal? (operator-subtype nf) wedge)
  (check-equal? (operator-arity nf) (exact-arity rank))
  (when name (check-equal? (expression (operator-name nf)) name))
  (when fields (check-equal? (eq-get nf 'argument-types) fields)))
(define the-tests
  (test-suite
   "calculus/form-field"
   (test-case
    "form-field"
    (define 1ff (procedure->1form-field 'proc))
    (check-ff 1ff)
    (check-ff (procedure->1form-field 'proc 'name) 'name))
   (test-case
    "ff:zero"
    (check-equal? (ff:zero (procedure->vector-field 'proc)) zero-manifold-function)
    (check-ff (ff:zero-like (procedure->1form-field 'proc)) 'ff:zero #:fields #f)
    (check-exn #px"ff:zero-like:\n\tassertion failed: " (λ () (ff:zero-like 'any)))
    (check-equal? (operator-procedure (ff:zero-like (procedure->1form-field 'proc))) ff:zero)
    (check-true (ff:zero? (ff:zero-like (procedure->1form-field 'proc))))
    (check-exn #px"ff:zero\\?:\n\tassertion failed: " (λ () (ff:zero? 'any))))
   (test-case
    "components->1form-field"
    (define x (λ (X) (ref X 0)))
    (define y (λ (X) (ref X 1)))
    (define ff1 (components->1form-field (up x y) R2-rect))
    (check-ff ff1 '(1form-field (up x y)))
    (define vf (components->vector-field (up (sin x) (sin y)) R2-rect))
    (check-simplified? ((ff1 vf) ((R2-rect '->point) #[a b]))
                       '(up (up (* a (sin a)) (* b (sin a))) (up (* a (sin b)) (* b (sin b)))))
    (define ff2 (components->1form-field (up y (sin x)) R2-rect 'test))
    (check-simplified? ((ff2 vf) ((R2-rect '->point) #[a b]))
                       '(up (up (* b (sin a)) (expt (sin a) 2)) (up (* b (sin b)) (* (sin a) (sin b)))))
    1form-field->components)
   (test-case
    "1form-field->components"
    (define x (λ (X) (ref X 0)))
    (define y (λ (X) (ref X 1)))
    (define ff1 (components->1form-field (up x y) R2-rect))
    (define cp (1form-field->components ff1 R2-rect))
    (check-simplified? (cp #(a b))
                       '(down (up (up a b) (up 0 0)) (up (up 0 0) (up a b))))
    (check-exn #px"Bad form field: 1form-field->components:\n\tassertion failed: "
               (λ () (1form-field->components 'any R2-rect))))
   (test-case
    "literal-1ff"
    (define ff1 (literal-1form-field 'X R1-rect))
    (check-ff ff1 'X)
    (check-simplified? ((ff1 (components->vector-field values R1-rect)) ((R1-rect '->point) 't))
                       '(down (* (X_0 t) t)))
    (define ff2 (literal-1form-field 'X R2-rect))
    (check-simplified? ((ff2 (components->vector-field values R2-rect)) ((R2-rect '->point) #(a b)))
                       '(+ (* a (X_0 (up a b))) (* b (X_1 (up a b))))))
   (test-case
    "coordinate-basis-1form-field"
    (define ff1 (coordinate-basis-1form-field R2-polar 'X))
    (check-ff ff1 'X)
    (check-simplified? ((ff1 (components->vector-field values R2-rect)) ((R2-rect '->point) #(a b)))
                       '(up (sqrt (+ (expt a 2) (expt b 2))) 0))
    (define ff3 (coordinate-basis-1form-field R3-rect 'X 1))
    (check-ff ff3 'X)
    (check-simplified? ((ff3 (components->vector-field values R3-rect)) ((R3-rect '->point) #(a b c)))
                       'b)
    (check-exn #px"Bad vector field: coordinate-basis-1form-field:\n\tassertion failed: "
               (λ () (ff3 'any))))
   (test-case
    "basis"
    (define x (λ (X) (ref X 0)))
    (define y (λ (X) (ref X 1)))
    (define vf (components->vector-field (up (sin x) (sin y)) R2-rect))
    (check-simplified? (((coordinate-system->1form-basis R2-rect) vf) ((R2-rect '->point) #[a b]))
                       '(up (sin a) (sin b)))
    (check-simplified? (((coordinate-system->1form-basis R2-rect) vf) ((R2-polar '->point) #[a b]))
                       '(up (sin (* a (cos b))) (sin (* a (sin b)))))
    
    (define fb (basis-components->1form-field (R2-rect '->coords) (coordinate-system->1form-basis R2-rect)))
    (check-ff fb)
    (check-simplified? ((fb vf) ((R2-rect '->point) #[a b]))
                       '(up (up (* a (sin a)) (* b (sin a))) (up (* a (sin b)) (* b (sin b)))))
    
    (define ff (components->1form-field (up x (* 2 y)) R2-rect))
    (define bc (1form-field->basis-components ff (coordinate-system->vector-basis R2-rect)))
    (check-simplified? (bc ((R2-rect '->point) #(a b)))
                       '(down (up (up a (* 2 b)) (up 0 0)) (up (up 0 0) (up a (* 2 b))))))
   (test-case
    "function->1form-field"
    (define x (λ (X) (ref X 0)))
    (define y (λ (X) (ref X 1)))
    (define vf (components->vector-field (up (sin x) (sin y)) R2-rect))
    (define (switch M) (define X ((R2-rect '->coords) M)) (up (ref X 1) (- (ref X 0))))
    (define ff (function->1form-field switch))
    (check-ff ff '(d switch))
    (check-simplified? ((ff vf) ((R2-rect '->point) #(a b)))
                       '(up (sin b) (* -1 (sin a))))
    (check-equal? function->1form-field differential-of-function))
   (test-case "rectangular"
              (install-coordinates R3-rect (up 'x 'y 'z))

              (define mr ((R3-rect '->point) (up 'x0 'y0 'z0)))

              (define a-1form
                (components->1form-field
                 (down (literal-function 'ax (-> (UP* Real) Real))
                       (literal-function 'ay (-> (UP* Real) Real))
                       (literal-function 'az (-> (UP* Real) Real)))
                 R3-rect))

              (define a-vector-field
                (components->vector-field
                 (up (literal-function 'vx (-> (UP* Real) Real))
                     (literal-function 'vy (-> (UP* Real) Real))
                     (literal-function 'vz (-> (UP* Real) Real)))
                 R3-rect))

              (check-simplified? ((a-1form a-vector-field) mr)
                                 '(+ (* (vx (up x0 y0 z0)) (ax (up x0 y0 z0)))
                                     (* (vy (up x0 y0 z0)) (ay (up x0 y0 z0)))
                                     (* (vz (up x0 y0 z0)) (az (up x0 y0 z0)))))

              (check-simplified? ((1form-field->components a-1form R3-rect) (up 'x0 'y0 'z0))
                                 '(down (ax (up x0 y0 z0)) (ay (up x0 y0 z0)) (az (up x0 y0 z0))))


              (install-coordinates R3-cyl (up 'r 'theta 'zeta))
              (check-simplified? ((1form-field->components a-1form R3-cyl)
                                  (up 'r0 'theta0 'z0))
                                 '(down
                                   (+ (* (cos theta0)
                                         (ax (up (* r0 (cos theta0)) (* r0 (sin theta0)) z0)))
                                      (* (sin theta0)
                                         (ay (up (* r0 (cos theta0)) (* r0 (sin theta0)) z0))))
                                   (+ (* r0 (cos theta0)
                                         (ay (up (* r0 (cos theta0)) (* r0 (sin theta0)) z0)))
                                      (* -1 r0 (sin theta0)
                                         (ax (up (* r0 (cos theta0)) (* r0 (sin theta0)) z0))))
                                   (az (up (* r0 (cos theta0)) (* r0 (sin theta0)) z0))))
              )
   (test-case "cylindrical"
              (define-coordinates (up x y z) R3-rect)
              (define-coordinates (up r theta zeta) R3-cyl)
              (define mr ((R3-rect '->point) (up 'x0 'y0 'z0)))
              (define mp ((R3-cyl '->point) (up 'r0 'theta0 'z0)))

              (check-equal? ((dx d/dx) mr) 1)
              (check-equal? ((dx d/dx) mp) 1)
              (check-simplified? ((1form-field->components dr R3-rect) (up 'x0 'y0 'z0))
                                 '(down (/ x0 (sqrt (+ (expt x0 2) (expt y0 2))))
                                        (/ y0 (sqrt (+ (expt x0 2) (expt y0 2))))
                                        0))
              (check-simplified? ((1form-field->components dtheta R3-rect) (up 'x0 'y0 'z0))
                                 '(down (/ (* -1 y0) (+ (expt x0 2) (expt y0 2)))
                                        (/ x0 (+ (expt x0 2) (expt y0 2)))
                                        0))
              (check-simplified? (((+ (* 'w_0 dr) (* 'w_1 dtheta)) (+ (* 'V^0 d/dx) (* 'V^1 d/dy)))
                                  mp)
                                 '(+ (* V^0 w_0 (cos theta0))
                                     (* V^1 w_0 (sin theta0))
                                     (/ (* -1 V^0 w_1 (sin theta0)) r0)
                                     (/ (* V^1 w_1 (cos theta0)) r0)))
              (check-simplified? (((components->1form-field (1form-field->components
                                                             (+ (* 'w_0 dr) (* 'w_1 dtheta))
                                                             R3-rect)
                                                            R3-rect)
                                   (+ (* 'V^0 d/dx) (* 'V^1 d/dy)))
                                  mp)
                                 '(+ (* V^0 w_0 (cos theta0))
                                     (* V^1 w_0 (sin theta0))
                                     (/ (* -1 V^0 w_1 (sin theta0)) r0)
                                     (/ (* V^1 w_1 (cos theta0)) r0)))

              (define counter-clockwise (- (* x d/dy) (* y d/dx)))
              (define outward (+ (* x d/dx) (* y d/dy)))
              (check-simplified? ((dx counter-clockwise) mr) '(* -1 y0))
              (check-simplified? ((dx outward) mr) 'x0)
              (check-simplified? ((dr counter-clockwise) mp) 0)
              (check-simplified? ((dr outward) mp) 'r0)
              (check-simplified? ((dr outward) mr) '(sqrt (+ (expt x0 2) (expt y0 2))))
              (check-simplified? (((* x dy) (+ (* 'u d/dx) (* 'v d/dy))) mr) '(* v x0))
              (check-simplified? ((dr d/dr) ((R3-rect '->point) (up 'x^0 'y^0 'z^0))) 1)
              (check-simplified? ((dr d/dtheta) ((R3-rect '->point) (up 'x^0 'y^0 'z^0))) 0)
              (check-simplified? ((dtheta d/dr) ((R3-rect '->point) (up 'x^0 'y^0 'z^0))) 0)
              (check-simplified? ((dtheta d/dtheta) ((R3-rect '->point) (up 'x^0 'y^0 'z^0))) 1))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))