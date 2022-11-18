#lang racket/base

(require rackunit
         "../../kernel/cstm/arity.rkt")

(define the-tests
  (test-suite
   "kernel/cstm/arity"
   (check-equal? (combine-arity 4 6) '(4 6))
   (check-equal? (combine-arity 4 (arity-at-least 6)) (list 4 (arity-at-least 6)))
   (check-equal? (combine-arity 4 (arity-at-least 5)) (arity-at-least 4))
   (check-equal? (combine-arity 4 (list 2 5)) (list 2 4 5))
   (check-equal? (combine-arity 4 (list 2 (arity-at-least 5))) (list 2 (arity-at-least 4)))
   (check-equal? (combine-arity 3 (list 2 (arity-at-least 5))) (list 2 3 (arity-at-least 5)))
   (check-equal? (combine-arity 3 (list 2 3 (arity-at-least 5))) (list 2 3 (arity-at-least 5)))
   (check-equal? (combine-arity (arity-at-least 4) (list 2 3 (arity-at-least 5))) (arity-at-least 2))

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
   (check-equal? (arity-intersect (arity-at-least 6) (list 2 6 8))
                 (vector (list 7 (arity-at-least 9)) '(6 8) 2))
  
   (check-equal? (arity-intersect (list 2 5 (arity-at-least 7)) (list 4 5 (arity-at-least 9)))
                 (vector (list 2 7 8) (list 5 (arity-at-least 9)) 4))
   (check-equal? (arity-intersect '(2 3) '(2 3)) #(() (2 3) ()))))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))