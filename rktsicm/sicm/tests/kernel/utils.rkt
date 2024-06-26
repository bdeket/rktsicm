#lang racket/base

(require rackunit
         "../../kernel/utils.rkt"
         "../helper.rkt"
         )

(provide the-tests)
(define the-tests
  (test-suite
   "kernel/utils"
   (test-case "ORIG:defer-application"
              (check-equal?
               ((((defer-application (lambda (x) (* 3 x))) (lambda (x) (+ x 2)))
                 (lambda (x) (/ x 2))) 3)
               21/2))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))