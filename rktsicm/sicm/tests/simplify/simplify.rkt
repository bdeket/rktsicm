#lang racket/base

(require rackunit
         "../../simplify/simplify.rkt"
         "../helper.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "simplify/simplify"
   (test-case
    "part 1"
    (void ((initializer rcf:analyzer)))

    (check-unique-match? ((expression-analyzer rcf:analyzer)
                          '(- i (* Is (- (exp (/ (- v2 v3) Vt)) 1))))
                         (kernel17)
                         `(+ (* (+ 1 (* -1 ,kernel17)) Is) i))
    (check-unique-match? ((auxiliary-variable-fetcher rcf:analyzer))
                         (kernel16 kernel17)
                         (list-no-order `(,kernel16 (/ (+ v2 (* -1 v3)) Vt))
                                        `(,kernel17 (exp ,kernel18)))
                         #:when (eq? kernel16 kernel18))
    (check-unique-match? ((expression-analyzer rcf:analyzer)
                          '(exp (/ (- v3 v2) (- Vt))))
                         (kernel17)
                         `(,@kernel17))
    (check-equal? ((expression-simplifier rcf:analyzer)
                   '(- i (* Is (- (exp (/ (- v2 v3) Vt)) 1))))
                  '(+ (* (+ 1 (* -1 (exp (/ (+ v2 (* -1 v3)) Vt)))) Is) i)))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))