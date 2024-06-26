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


   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))