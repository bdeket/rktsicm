#lang racket/base

(require rackunit
         "../../generic.rkt"
         "../../rkt/default-object.rkt"
         "../helper.rkt")

(define D derivative)
(provide the-tests)
(define the-tests
  (test-suite
   "kernel/deriv"
   (test-case "ORIG:simple-derivative-internal"
              (check-equal? (simplify ((simple-derivative-internal
                                        (lambda (eps)
                                          (lambda (t)
                                            ((D (* cos eps)) t)))
                                        'e)
                                       't))
                            '(* -1 (sin t))))

   (test-case "ORIG:derivative"
              (check-equal? (simplify (((D (lambda (eps)
                                             (lambda (t)
                                               ((D (* cos eps)) t))))
                                        'e)
                                       't))
                            '(* -1 (sin t))))
   ;; MAIN
   (test-case
    "deriv:euclidean-structure"
    ;; deriv:euclidean-structure works on 1-arg functions
    ;; if the arg is a structure, act as if the elements of the structure are variables
    (check-equal? ((deriv:euclidean-structure (λ (a) (* 3 a a)) '()) (up 0 1))
                  (down (up (up 0 3) (up 3 0)) (up (up 0 0) (up 0 6))))
    ;; else the arg is the variable
    (check-equal? ((deriv:euclidean-structure (λ (a) (* 3 a a)) '()) 2)
                  12)
    ;; if a selector is given, only look at the selector
    (check-equal? ((deriv:euclidean-structure (λ (a) (* 3 a a)) '(1)) (up 0 1))
                  (up (up 0 0) (up 0 6)))
    (check-equal? ((deriv:euclidean-structure (λ (a) (* 3 a a)) '(1 0)) (up (down 0 1) (down 'c 3)))
                  (up (down 3 0) (down 9 3)))
    (skip
     ;; TODO: this seems broken => (abstract-down 'c) is not an abstract-quantity?
     ;; probably in kernel/types, g:type should return a 'make-type' structure and use the
     ;; apropriate abstract quantity
     (check-equal? ((deriv:euclidean-structure (λ (a) (* 3 a a)) '(1)) (up (down 0 1) (abstract-down 'c)))
                  (up (down 3 0) (down 9 3))))
    ;; deeper elements still can only be structures
    (check-exn #px"Bad structure -- DERIV:EUCLIDEAN-STRUCTURE"
               (λ () ((deriv:euclidean-structure (λ (a) (* 3 a a)) '(1))
                      (up (down 0 1) (matrix-by-rows '(1 2))))))
    ;; but selectors only work for structure-args
    (check-exn #px"Bad selectors -- DERIV:EUCLIDEAN-STRUCTURE"
               (λ () ((deriv:euclidean-structure (λ (a) (* 3 a a)) '(1 0)) 2))))
   (test-case
    "deriv:multivariate-derivative"
    ;; multi-derivative is implemented in function of structures
    (check-equal? ((deriv:multivariate-derivative (λ () 1) '()))
                  0)
    (check-equal? ((deriv:multivariate-derivative (λ (x) x) '()) 't)
                  1)
    (check-equal? ((deriv:multivariate-derivative (λ (x . rst) (g:*:n (cons x rst))) '()) 't 'u 'v)
                  (down (* 'u 'v) (* 't 'v) (* 't 'u)))
    (check-equal? ((deriv:multivariate-derivative (λ (x y) (* x y)) '()) 't 'u)
                  (down 'u 't))
    (check-equal? ((deriv:multivariate-derivative (λ (x y . rst) (g:*:n (list* x y rst))) '()) 't 'u 'v)
                  (down (* 'u 'v) (* 't 'v) (* 't 'u)))
    (check-equal? ((deriv:multivariate-derivative (λ (x y . rst) (g:*:n (list* x y rst))) '(1)) 't 'u 'v)
                  (* 't 'v))
    (check-equal? ((deriv:multivariate-derivative (λ (x y z) (g:* x y z)) '()) 't 'u 'v)
                  (down (* 'u 'v) (* 't 'v) (* 't 'u)))
    (check-equal? ((deriv:multivariate-derivative (λ (x y z) (g:* x y z)) '(0)) 't 'u 'v)
                  (* 'u 'v))
    (check-equal? ((deriv:multivariate-derivative (λ (x y z . rst) (g:*:n (list* x y z rst))) '()) 't 'u 'v)
                  (down (* 'u 'v) (* 't 'v) (* 't 'u)))
    (check-equal? ((deriv:multivariate-derivative (λ (x [y default-object]) (if (default-object? y) x (g:* x y))) '()) 't)
                  1)
    (check-equal? ((deriv:multivariate-derivative (λ (x [y default-object]) (g:* x y)) '()) 't 'u)
                  (down 'u 't))
    (check-equal? ((deriv:multivariate-derivative (λ (x y [z default-object]) (if (default-object? z) (g:* x y) (g:* x y z))) '()) 't 'u)
                  (down 'u 't))
    (check-equal? ((deriv:multivariate-derivative (λ (x [y default-object] [z default-object])
                                                    (if (default-object? z)
                                                        (if (default-object? y) x (g:* x y))
                                                        (if (default-object? y) (g:* x z) (g:* x y z)))) '()) 't)
                  1)
    (check-equal? ((deriv:multivariate-derivative (λ ([x default-object]) (if (default-object? x) 10 x)) '()) 't)
                  1)
    (check-equal? ((deriv:multivariate-derivative (λ ([x default-object]) (if (default-object? x) 10 x)) '()))
                  0)
    (check-exn #px"Wrong number of args passed to derivative with arity"
               (λ () ((deriv:multivariate-derivative (λ (x y [z 0]) (g:* x y z)) '()))))
    )

   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))