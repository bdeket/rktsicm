#lang racket/base

(require rackunit
         "../../general/equals.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "general/assert"
   (test-case
    "pair:eq?"
    (check-true (pair:eq? (cons 1 2) (cons 1 2)))
    (define A (list 1))
    (check-true (pair:eq? (cons A 2) (cons A 2)))
    (check-true (pair:eq? '(2) '(2)))
    (check-false (pair:eq? (cons (list 2) 1) (cons (list 2) 1)))
    )
   (test-case
    "vector:equal?"
    (check-true (vector:equal? (vector 1 2) (vector 1 2)))
    (check-false (vector:equal? (vector 1 2) (vector 1 2 3)))
    (check-false (vector:equal? (vector 1 2 (hash 1 2)) (vector 1 2 (hash 1 2))))
    )
   (test-case
    "simple:equal?"
    (check-true (simple:equal? 1 1))
    (check-true (simple:equal? 2. (+ 1 1.)))

    (check-true (simple:equal? "string" "string"))
    (check-false (simple:equal? "string" "String"))

    (check-true (simple:equal? (list 1) (list 1)))
    (check-true (simple:equal? (list (list 1)) (list (list 1))))
    (check-true (simple:equal? (list (list (string-append "a" "b")))
                               (list (list (string-append "a" "b")))))

    (check-true (simple:equal? (vector 1) (vector 1)))
    (check-true (simple:equal? (vector (vector 1)) (vector (vector 1))))
    (check-true (simple:equal? (vector (vector "1")) (vector (vector "1"))))
    (check-true (simple:equal? (vector (list 1)) (vector (list 1))))
    (check-true (simple:equal? (list (vector 1)) (list (vector 1))))
    
    (define A (hash 1 2))
    (check-true (simple:equal? A A))
    (check-true (simple:equal? (list (vector A)) (list (vector A))))
    (check-false (simple:equal? (hash 1 2) (hash 1 2)))
    )
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))