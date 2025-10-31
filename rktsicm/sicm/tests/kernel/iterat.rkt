#lang racket/base

(require rackunit
         "../../kernel/iterat.rkt"
         "../helper.rkt"
         )

(provide the-tests)
(define the-tests
  (test-suite
   "kernel/iterat"
   (test-case
    "renames"
    (check-equal? generate-list build-list)
    (check-equal? generate-vector build-vector))
   (test-case
    "subst-coord"
    (check-equal? (list-with-substituted-coord '(0 1 2 3 4 5 6 7 8 9) 3 'a)
                  '(0 1 2 a 4 5 6 7 8 9))
    (check-equal? (vector-with-substituted-coord #(0 1 2 3 4 5 6 7 8 9) 3 'a)
                  #(0 1 2 a 4 5 6 7 8 9))
    (define A #(0 1 2 3 4 5 6 7 8 9))
    (define B (vector-with-substituted-coord A 3 'a))
    (check-equal? (vector-ref A 3) 3))
   (test-case
    "vector-elementwise"
    (check-equal? ((vector-elementwise +)) #())
    (check-equal? ((vector-elementwise +) #(0 1 2) #(3 2 1)) #(3 3 3))
    (check-exn #px"vector-elementwise:\\+: contract violation\\\n  expected: same-length vectors\\\n  given:"
               (λ () ((vector-elementwise +) #(0 1 2) #(3 2))))
    (check-exn #px"vector-elementwise:\\+: contract violation\\\n  expected: same-length vectors\\\n  given:"
               (λ () ((vector-elementwise +) #(0 1) #(3 2 1)))))
   (test-case
    "vector-forall"
    (check-true  (vector-forall =))
    (check-true  (vector-forall = #()))
    (check-true  (vector-forall = #(1) #(1) #(1)))
    (check-true  (vector-forall = #(1 2 3) #(1 2 3)))
    (check-false (vector-forall = #(1 2 3) #(1 2 4)))
    (check-exn #px"vector-forall: contract violation\\\n  expected: same-length vectors\\\n  given:"
               (λ () (vector-forall = #(0 1 2) #(3 2))))
    (check-exn #px"vector-forall: contract violation\\\n  expected: same-length vectors\\\n  given:"
               (λ () (vector-forall = #(0 1) #(3 2 1)))))
   (test-case
    "vector-exists"
    (check-false (vector-exists =))
    (check-false (vector-exists = #()))
    (check-true  (vector-exists = #(1) #(1) #(1)))
    (check-true  (vector-exists = #(1 2 3) #(1 2 3)))
    (check-true  (vector-exists = #(1 2 3) #(1 2 4)))
    (check-exn #px"vector-exists: contract violation\\\n  expected: same-length vectors\\\n  given:"
               (λ () (vector-exists = #(0 1 2) #(3 2))))
    (check-exn #px"vector-exists: contract violation\\\n  expected: same-length vectors\\\n  given:"
               (λ () (vector-exists = #(0 1) #(3 2 1)))))
   (test-case
    "vector-accumulate"
    (check-equal? (vector-accumulate max abs 0 #()) 0)
    (check-equal? (vector-accumulate max abs 0 #(1)) 1)
    (check-equal? (vector-accumulate max abs 0 #(1 -2)) 2)
    (check-equal? (vector-accumulate + abs #f #(1 -2)) 3))
   ;; ARRAYS
   (test-case
    "generate"
    (check-equal? (generate-array 2 3 list)
                  (array-by-rows '(((0 0)(0 1)(0 2))((1 0)(1 1)(1 2)))))
    (check-equal? (generate-array 2 3 list)
                  (array-by-cols '(((0 0)(1 0))((0 1)(1 1))((0 2)(1 2))))))
   (test-case
    "array-ref"
    (check-equal? (array-ref (generate-array 3 3 cons) 2 1) (cons 2 1))
    (check-equal? (num-rows (generate-array 8 2 +)) 8)
    (check-equal? (num-rows (generate-array 0 0 +)) 0)
    (check-equal? (num-cols (generate-array 3 7 +)) 7)
    (check-equal? (num-cols (generate-array 0 0 +)) 0)
    (check-equal? (nth-row (generate-array 3 2 +) 1) #(1 2))
    (check-equal? (nth-col (generate-array 3 2 +) 1) #(1 2 3)))
   (test-case
    "set!/copy"
    (let ([A (generate-array 3 3 cons)])
      (array-set! A 2 2 'c)
      (define B (array-copy A))
      (array-set! A 0 1 'a)
      (check-equal? (array-ref A 2 1) (cons 2 1))
      (check-equal? (array-ref A 0 1) 'a)
      (check-equal? (array-ref B 2 2) 'c)))
   (test-case
    "transpose"
    (check-equal? (transpose-array (generate-array 2 3 +))
                  (generate-array 3 2 +)))
   (test-case
    "substituted"
    (check-equal? (array-with-substituted-row (generate-array 2 3 +) 0 #(a b c))
                  (array-by-rows '((a b c)(1 2 3))))
    (check-equal? (array-with-substituted-col (generate-array 2 3 +) 0 #(a b))
                  (array-by-rows '((a 1 2)(b 2 3))))
    ;; TODO : check that new row/col is correct size ?
    (skip (check-exn #px"---"
                     (λ () (array-with-substituted-row (generate-array 2 3 +) 0 #(a))))
          (check-exn #px"---"
                     (λ () (array-with-substituted-col (generate-array 2 3 +) 0 #(a b c))))))
   (test-case
    "elementwise"
    (check-equal? ((array-elementwise +) (generate-array 2 2 +) (array-by-rows '((3 -1)(2 8))))
                  (array-by-rows '((3 0)(3 10))))
    ;; TODO : check arrays are compatible ?
    (skip (check-exn #px"---"
                     (λ () ((array-elementwise +) (generate-array 2 2 +) (generate-array 3 3 +))))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))