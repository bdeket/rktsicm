#lang racket/base

(require rackunit
         "../../main.rkt"
         (only-in "../../kernel/extapply.rkt" with-literal-reconstruction-enabled)
         )

(provide the-tests)
(define the-tests
  (test-suite
   "kernel/extapply"
   (test-case "ORIG:with-self-evaluating-unbound-variables"
              (check-equal? (simplify
                             (with-self-evaluating-unbound-variables
                                 (λ () (+ a 1))))
                            '(+ 1 a)))

   (test-case "ORIG:with-literal-apply-enabled"
              (let ()
                (check-exn exn:fail?
                           (λ () (with-self-evaluating-unbound-variables
                                     (λ () (+ (f 'a) 1)))))

                (check-equal? (simplify
                               (with-literal-apply-enabled
                                   (λ ()
                                     (with-self-evaluating-unbound-variables
                                         (λ () (+ (f 'a) 3))))))
                              '(+ 3 (f a)))))
   (test-case "#%app"
              (parameterize ([*enable-generic-apply* #t])
                (check-equal? ((λ (x y) (+ x y)) 2 3) 5))
              (parameterize ([*enable-generic-apply* #f])
                (check-equal? ((λ (x y) (+ x y)) 2 3) 5))
              (parameterize ([*enable-generic-apply* #t])
                (check-equal? ((vector (λ (x y) (+ x y))) 2 3) #(5)))
              (parameterize ([*enable-generic-apply* #f])
                (check-exn #px"application: not a procedure;\\\n expected a procedure that can be applied to arguments\\\n  given:" (λ () ((vector (λ (x y) (+ x y))) 2 3)))))
   (test-case
    "evaluate once only"
    (define x 0)
    (check-equal? ((begin (set! x (+ x 1)) +) 3 4) 7)
    (check-equal? x 1))
   (test-case
    "literal-reconstruction"
    (check-equal? (expression (literal-function 'F)) 'F)
    (check-equal? (with-literal-reconstruction-enabled (λ () (expression (literal-function 'F))))
                  '(literal-function 'F (-> Real Real))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))