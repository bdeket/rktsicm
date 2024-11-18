#lang racket/base

(require rackunit
         (submod "../../general/table.rkt" ALL))

(provide the-tests)
(define the-tests
  (test-suite
   "general/table"
   (test-case
    "TABLE - make-table"
    (define table (make-table 'test assq))
    (check-equal? (vector-ref table 0) 'test)
    (check-true (no-value? (car ((vector-ref table 5))))))
   (test-case
    "TABLE - get / getter / put! / putter!"
    (define table (make-table 'test assq))
    (check-true (no-value? (get table 'A)))
    (put! table 1 'A)
    (check-equal? (get table 'A) 1)
    (put! table 2 'B 'C)
    (check-equal? (get table 'B 'C) 2)
    (check-true (no-value? (get table 'B)))
    (define TG (getter table))
    (define TP (putter! table))
    (TP 3 'B 'C 'D)
    (check-equal? (TG 'B 'C) 2)
    (check-equal? (TG 'B 'C 'D) 3))
   (test-case
    "TABLE - get-with-default / getter"
    (define table (make-table 'test assq))
    (put! table 1 'A)
    (check-equal? (get-with-default table 'default 'B) 'default)
    (define TG (getter-with-default table 'default))
    (check-equal? (TG 'A) 1)
    (check-equal? (TG 'C) 'default))
   (test-case
    "TABLE - get-with-check / getter"
    (define table (make-table 'test assq))
    (put! table 1 'A)
    (check-exn #px"can't find value in table" (λ () (get-with-check table 'B)))
    (define TG (getter-with-check table))
    (check-equal? (TG 'A) 1)
    (check-exn #px"can't find value in table" (λ () (TG 'C))))
   (test-case
    "TABLE - add-to-list!"
    (define table (make-table 'test assq))
    (put! table 1 'A)
    (add-to-list! 2 table 'A)
    (check-equal? (get table 'A) '(2 . 1))
    (add-to-list! 5 table 'B 'C)
    (add-to-list! 6 table 'B 'C)
    (check-equal? (get table 'B 'C) '(6 5)))
   (test-case
    "TABLE - adjoin-to-list!"
    (define table (make-table 'test assq))
    (adjoin-to-list! 2 table 'A)
    (adjoin-to-list! 3 table 'A)
    (adjoin-to-list! 2 table 'A)
    (adjoin-to-list! 5 table 'A)
    (check-equal? (get table 'A) '(5 3 2)))
   (test-case
    "TABLE - store!"
    (define table (make-table 'test assq))
    (store! 2 table 'A 'B)
    (check-equal? (get table 'A 'B) 2))
   
   (test-case
    "AList - lookup"
    (check-exn #px"key not in table -- LOOKUP" (λ () (lookup 'A '())))
    (check-equal? (lookup 'A '((A 1)(B 2))) 1)
    (check-equal? (lookup 'B '((A 1)(B 2))) 2)
    (check-exn #px"key not in table -- LOOKUP" (λ () (lookup 'C '((A 1)(B 2))))))
   (test-case
    "AList - rlookup"
    (check-false (rlookup 'A '()))
    (check-equal? (rlookup 'A '((1 A)(3)(2 B))) '(1 A))
    (check-equal? (rlookup 'B '((1 A)(3)(2 B))) '(2 B))
    (check-false (rlookup 'C '((1 A)(2 B)))))
   (test-case
    "AList - rassq"
    (check-false (rassq 'A '()))
    (check-equal? (rassq 'A '((1 . A)(2 . B))) '(1 . A))
    (check-equal? (rassq 'B '((1 . A)(2 . B))) '(2 . B))
    (check-false (rassq 'C '((1 . A)(2 . B))))
    (check-false (rassq (list 'A) '((1 A)(2 . B)))))
   (test-case
    "AList - rassoc"
    (check-false (rassoc 'A '()))
    (check-equal? (rassoc 'A '((1 . A)(2 . B))) '(1 . A))
    (check-equal? (rassoc 'B '((1 . A)(2 . B))) '(2 . B))
    (check-false (rassoc 'C '((1 . A)(2 . B))))
    (check-equal? (rassoc (list 'A) '((1 A)(2 . B))) '(1 A)))
   (test-case
    "AList - disassoc"
    (check-equal? (disassoc 'A '()) '())
    (check-equal? (disassoc 'A '((B 2)(A 1))) '((B 2))))

   
   (test-case
    "PList - default-lookup"
    (check-equal? (default-lookup 'name 'default '()) 'default)
    (check-equal? (default-lookup 'name 'default '(A B C D)) 'default)
    (check-equal? (default-lookup 'A 'default '(A B C D)) 'B)
    (check-equal? (default-lookup 'C 'default '(A B C D)) 'D))
   (test-case
    "table-of"
    (check-equal? ((table-of eq? '(A B) '(1 2)) 'A) 1)
    (check-equal? ((table-of eq? '(A B) '(1 2)) 'B) 2)
    (check-exn #px"Key not in table" (λ () ((table-of eq? '(A B) '(1 2)) 'C))))

   
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))