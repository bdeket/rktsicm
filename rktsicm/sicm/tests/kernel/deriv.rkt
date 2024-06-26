#lang racket/base

(require rackunit
         "../../main.rkt"
         "../../mechanics/universal.rkt"
         )

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

   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))