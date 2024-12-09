#lang racket/base

(require rackunit
         "../../../kernel/cstm/arity.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "kernel/cstm/arity"
   (test-case
    "exact-arity"
    (check-equal? (exact-arity 1) 1)
    (check-equal? (exact-arity 0) 0)
    (check-exn #px"exact-arity: contract violation\n  expected: exact-positive-integer?" (λ () (exact-arity 4.2)))
    (check-exn #px"exact-arity: contract violation\n  expected: exact-positive-integer?" (λ () (exact-arity 4.0)))
    (check-exn #px"exact-arity: contract violation\n  expected: exact-positive-integer?" (λ () (exact-arity -1))))
   (test-case
    "exactly-n?"
    (check-true  (exactly-n? (exact-arity 1)))
    (check-false (exactly-n? (arity-at-least 2))))
   (test-case
    "any-number?"
    (check-true  (any-number? (arity-at-least 0)))
    (check-false (any-number? (arity-at-least 1)))
    (check-false (any-number? (exact-arity 0))))
   (test-case
    "artity-min"
    (check-equal? (arity-min (exact-arity 1)) 1)
    (check-equal? (arity-min (arity-at-least 3)) 3)
    (check-equal? (arity-min (procedure-arity (case-lambda [(x y) 2][(a b c d . x) 4]))) 2))
   (test-case
    "joint-arity"
    (check-equal? (joint-arity (exact-arity 2) (exact-arity 2)) (exact-arity 2))
    (check-equal? (joint-arity (exact-arity 2) (exact-arity 6)) #f)
    
    (check-equal? (joint-arity (exact-arity 2) (arity-at-least 4)) #f)
    (check-equal? (joint-arity (arity-at-least 4) (exact-arity 2)) #f)
    (check-equal? (joint-arity (exact-arity 2) (arity-at-least 1)) 2)
    (check-equal? (joint-arity (arity-at-least 1) (exact-arity 2)) 2)
    (check-equal? (joint-arity (arity-at-least 2) (arity-at-least 1)) (arity-at-least 2))
    (check-equal? (joint-arity (arity-at-least 1) (arity-at-least 2)) (arity-at-least 2))

    (check-equal? (joint-arity 4 '(2 5 7)) #f)
    (check-equal? (joint-arity '(2 5 7) 4) #f)
    (check-equal? (joint-arity 4 '(2 4 7)) 4)
    (check-equal? (joint-arity '(2 4 7) 4) 4)
    (check-equal? (joint-arity (arity-at-least 9) '(2 4 7)) #f)
    (check-equal? (joint-arity '(2 4 7) (arity-at-least 9)) #f)
    (check-equal? (joint-arity (arity-at-least 4) '(2 4 7)) '(4 7))
    (check-equal? (joint-arity '(2 4 7) (arity-at-least 4)) '(4 7))
    (check-equal? (joint-arity '(2 4 7) '(3 5 8)) #f)
    (check-equal? (joint-arity '(3 5 8) '(2 4 7)) #f)
    (check-equal? (joint-arity '(2 4 7) '(2 3 7)) '(2 7))
    (check-equal? (joint-arity '(2 3 7) '(2 4 7)) '(2 7))
    (check-equal? (joint-arity `(2 4 ,(arity-at-least 7)) `(3 5 8)) 8)
    (check-equal? (joint-arity `(3 5 8) `(2 4 ,(arity-at-least 7))) 8)
    (check-equal? (joint-arity `(2 4 ,(arity-at-least 7)) `(3 5 ,(arity-at-least 8))) (arity-at-least 8))
    (check-equal? (joint-arity `(3 5 ,(arity-at-least 8)) `(2 4 ,(arity-at-least 7))) (arity-at-least 8))
    (check-equal? (joint-arity `(2 4 ,(arity-at-least 7)) `(2 3 ,(arity-at-least 8))) `(2 ,(arity-at-least 8)))
    (check-equal? (joint-arity `(2 4 ,(arity-at-least 7)) `(2 ,(arity-at-least 3))) `(2 4 ,(arity-at-least 7)))
    (check-equal? (joint-arity #f 3) #f))
   
   (test-case
    "combine-arity"
    (check-exn #px"combine-arity: contract violation\n  expected: procedure-arity?" (λ () (combine-arity #f 3)))
    (check-exn #px"combine-arity: contract violation\n  expected: procedure-arity?" (λ () (combine-arity 3 #f)))
    (check-equal? (combine-arity 4 6) '(4 6))
    (check-equal? (combine-arity 4 (arity-at-least 6)) (list 4 (arity-at-least 6)))
    (check-equal? (combine-arity 4 (arity-at-least 5)) (arity-at-least 4))
    (check-equal? (combine-arity 4 (list 2 5)) (list 2 4 5))
    (check-equal? (combine-arity 4 (list 2 (arity-at-least 5))) (list 2 (arity-at-least 4)))
    (check-equal? (combine-arity 3 (list 2 (arity-at-least 5))) (list 2 3 (arity-at-least 5)))
    (check-equal? (combine-arity 3 (list 2 3 (arity-at-least 5))) (list 2 3 (arity-at-least 5)))
    (check-equal? (combine-arity (arity-at-least 4) (list 2 3 (arity-at-least 5))) (arity-at-least 2)))

   (test-case
    "arity-intersect"
    (check-equal? (arity-intersect '() 3) #(() () 3))
    (check-equal? (arity-intersect '() (arity-at-least 3)) (vector '() '() (arity-at-least 3)))
    (check-equal? (arity-intersect '() (list 2 3 (arity-at-least 3))) (vector '() '() (arity-at-least 2)))

    (check-equal? (arity-intersect 4 5) #(4 () 5))
    (check-equal? (arity-intersect 4 4) #(() 4 ()))
    (check-equal? (arity-intersect 4 (arity-at-least 3))
                  (vector '() 4 (list 3 (arity-at-least 5))))
    (check-equal? (arity-intersect (arity-at-least 3) 4)
                  (vector (list 3 (arity-at-least 5)) 4 '()))
    (check-equal? (arity-intersect 2 '(2 4)) #(() 2 4))
    (check-equal? (arity-intersect 4 (list 1 (arity-at-least 3)))
                  (vector '() 4 (list 1 3 (arity-at-least 5))))
    (check-equal? (arity-intersect 4 (list 1 3 (arity-at-least 6)))
                  (vector 4 '() (list 1 3 (arity-at-least 6))))

    (check-equal? (arity-intersect (arity-at-least 2) (arity-at-least 2))
                  (vector '() (arity-at-least 2) '()))
    (check-equal? (arity-intersect (arity-at-least 5) (arity-at-least 7))
                  (vector (list 5 6) (arity-at-least 7) '()))
    (check-equal? (arity-intersect (arity-at-least 6) (arity-at-least 7))
                  (vector 6 (arity-at-least 7) '()))
    (check-equal? (arity-intersect (arity-at-least 7) (arity-at-least 5))
                  (vector '() (arity-at-least 7) '(5 6)))
    (check-equal? (arity-intersect (arity-at-least 6) (list 2 6 8))
                  (vector (list 7 (arity-at-least 9)) '(6 8) 2))
  
    (check-equal? (arity-intersect (list 2 5 (arity-at-least 7)) (list 4 5 (arity-at-least 9)))
                  (vector (list 2 7 8) (list 5 (arity-at-least 9)) 4))
    (check-equal? (arity-intersect (list 2 5 (arity-at-least 7)) (arity-at-least 4))
                  (vector 2 (list 5 (arity-at-least 7)) '(4 6)))
    (check-equal? (arity-intersect '(2 3) '(2 3)) #(() (2 3) ())))))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))