#lang racket/base

(require rackunit
         "../../simplify/default.rkt"
         "../../units/SI-units.rkt"
         "../../kernel-intr.rkt"
         "../helper.rkt")

(define c²+s² '(+ (expt (cos x) 2) (expt (sin x) 2)))
(provide the-tests)
(define the-tests
  (test-suite
   "simplify/default"
   ;; the default-handler is the only one that really simplifies,
   ;; the others just translate to an expression
   (test-case
    "default-simplify"
    (define-values (assign-operation s:assign-operations)
      (make-assign-operations 'simplify))
    (assign-operation 'simplify (λ (x) (and x (if (and (pair? x) (not (list? x))) x `(xpress ,x)))))
    (s:assign-operations #t)
    
    (check-equal? (default-simplify 'top-expr) 'top-expr)
    (check-equal? (default-simplify '(car . cdr)) `((xpress car) . (xpress cdr)))
    (check-equal? (default-simplify '(first #f (+ (expt (cos x) 2) (expt (sin x) 2))))
                  '((xpress first) #f (xpress (+ (expt (cos x) 2) (expt (sin x) 2)))))
    (check-equal? (default-simplify '(first (1 2 . 3) (+ (expt (cos x) 2) (expt (sin x) 2))))
                  '((xpress first) (1 2 . 3) (xpress (+ (expt (cos x) 2) (expt (sin x) 2)))))
    (check-equal? (default-simplify `(first 3 ,c²+s²))
                  '((xpress first) (xpress 3) (xpress 1))))
   
   (test-case
    "unit"
    (check-equal? (simplify-units (g:* 5 &meter))
                  '(& (xpress 5) &meter)))
   (test-case
    "procedure / litfun / operator"
    ;; get object-name
    (check-equal? (simplify-procedure symb:sin) 'symb:sin)
    ;; but don't do this on abstract functions or operators
    (check-equal? (simplify-procedure (literal-function 'this-literal-function))
                  'literal-function)
    (check-not-equal? (simplify-procedure (o:+ o:identity o:identity)) o:+)
    (check-equal? (simplify-abstract-function (literal-function 'this-literal-function))
                  '(xpress this-literal-function))
    (check-equal? (simplify-operator (o:+ o:identity o:identity))
                  '(xpress (+ identity identity))))
   (test-case
    "quaternion, matrix, diff, struct ..."
    (check-equal? (simplify-quaternion (quaternion 1 2 3 4))
                  `(quaternion (xpress 1) (xpress 2) (xpress 3) (xpress 4)))
    (check-equal? (simplify-matrix (matrix-by-rows '(1 2)))
                  `(matrix-by-rows (list (xpress 1) (xpress 2))))
    (check-equal? (simplify-differential (make-differential-quantity (list (make-differential-term 'a 'b))))
                  '(make-differential-quantity (list (make-differential-term 'a (xpress b)))))
    (check-equal? (simplify-down (down 2 3))
                  '(down (xpress 2) (xpress 3)))
    (check-equal? (simplify-up (up 2 3))
                  '(up (xpress 2) (xpress 3)))
    (check-equal? (simplify-up (vector 2 3))
                  '(up (xpress 2) (xpress 3)))
    (check-equal? (simplify-literal-number (literal-number 'a))
                  'a))
   (test-case
    "now with default"
    (simplify:assign-operations #t)
    (check-equal? (g:simplify symb:sin) 'symb:sin)
    (check-equal? (g:simplify (literal-function 'f)) 'f)
    (check-equal? (g:simplify (o:+ o:identity o:identity)) '(* 2 identity))
    (check-equal? (g:simplify (quaternion c²+s² 2 3 4))
                  `(quaternion 1 2 3 4))
    (check-equal? (g:simplify (matrix-by-rows (list c²+s² 2)))
                  '(matrix-by-rows (list 1 2)))
    (check-equal? (g:simplify (make-differential-quantity (list (make-differential-term 'a c²+s²))))
                  '(make-differential-quantity (list (make-differential-term 'a 1))))
    (check-equal? (g:simplify (down c²+s² 3))
                  '(down 1 3))
    (check-equal? (g:simplify (up c²+s² 3))
                  '(up 1 3))
    (check-equal? (g:simplify (vector c²+s² 3))
                  '(up 1 3))
    (check-equal? (g:simplify (literal-number 'a))
                  'a)

    (check-equal? (g:simplify (g:* 5 &meter))
                  '(& 5 &meter)))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))