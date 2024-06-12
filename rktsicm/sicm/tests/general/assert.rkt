#lang racket/base

(require rackunit
         "../../general/assert.rkt")

(define the-tests
  (test-suite
   "general/assert"
   (test-case
    "single arg check"
    (define PX #px"assertion failed: #f")
    (check-not-exn     (λ () (assert (list? '()))))
    (check-not-exn              (λ () (assert #t)))
    (check-exn PX               (λ () (assert #f)))
    (check-exn exn:fail? (λ () (assert (list? #()))))
    )
   (test-case
    "double arg check"
    (define PX #px"msg:\\\n\\\tassertion failed: #f")
    (check-not-exn       (λ () (assert (list? '()) "msg")))
    (check-not-exn                (λ () (assert #t "msg")))
    (check-exn PX                 (λ () (assert #f "msg")))
    (check-exn exn:fail? (λ () (assert (list? #()) "msg"))))
   (test-case
    "triple arg check"
    (define PX #px"\\(fct 1 2\\): msg")
    (check-not-exn       (λ () (assert (list? '()) "msg" "fct" 1 2)))
    (check-not-exn                (λ () (assert #t "msg" "fct" 1 2)))
    (check-exn PX                 (λ () (assert #f "msg" "fct" 1 2)))
    (check-exn exn:fail? (λ () (assert (list? #()) "msg" "fct" 1 2))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))