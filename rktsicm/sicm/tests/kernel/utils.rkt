#lang racket/base

(require rackunit
         "../../kernel/utils.rkt"
         "../helper.rkt"
         )

(define kernel-tests
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
  (run-tests kernel-tests))