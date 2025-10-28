#lang racket/base

(require rackunit
         "../../main.rkt"
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
   (test-case "evaluate once only"
              (define x 0)
              (check-equal? ((begin (set! x (+ x 1)) +) 3 4) 7)
              (check-equal? x 1))


   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))