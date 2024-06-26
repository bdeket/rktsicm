#lang racket/base

(require rackunit
         "../../main.rkt"
         "../../mechanics/universal.rkt"
         )

(provide the-tests)
(define the-tests
  (test-suite
   "kernel/diff"
   (test-case "ORIG:deriv"
              (let ()
                (define (((f x) g) y)
                  (g (+ x y)))

                (define f-hat ((D f) 3))

                (check-= ((f-hat exp) 5)
                         2980.9579870417283
                         1e-10)

                (check-= ((f-hat (f-hat exp)) 5)
                         59874.14171519782
                         1e-10)))


   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))