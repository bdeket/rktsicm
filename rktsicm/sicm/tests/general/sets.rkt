#lang racket/base

(require rackunit
         (submod "../../general/sets.rkt" ALL))

(provide the-tests)
(define (set-package-test S e1 e2 e3 e4)
  (check-true ((empty-set? S) (empty-set S)))
  (check-true ((empty-set? S) ((list->set S) '())))
  
  (check-equal? ((singleton-set S) e1) ((singleton-set S) e1))
  (check-true ((singleton-set? S) ((singleton-set S) e2)))
  (check-false ((singleton-set? S) (empty-set S)))
  (check-false ((singleton-set? S) ((list->set S) (list e1 e2))))

  (check-equal? ((list->set S) (list e1)) ((singleton-set S) e1))
  (check-equal? ((set->list S) ((list->set S) (list e3 e2 e1))) (list e1 e2 e3))

  (check-equal? ((adjoin-set S) e2 ((singleton-set S) e1))
                ((union-sets S) ((singleton-set S) e2) ((singleton-set S) e1)))
  (check-equal? ((adjoin-set S) e2 ((singleton-set S) e1))
                ((union-sets S) ((singleton-set S) e1) ((singleton-set S) e2)))
  (check-equal? ((adjoin-set S) e2 ((singleton-set S) e1))
                ((list->set S) (list e2 e1)))
  (check-equal? ((adjoin-set S) e2 ((singleton-set S) e1))
                ((list->set S) (list e1 e2 e2)))
  
  (check-equal? ((remove-set S) e2 ((adjoin-set S) e2 ((singleton-set S) e1)))
                ((singleton-set S) e1))
  (check-equal? ((remove-set S) e2 (empty-set S))
                (empty-set S))
  (check-equal? ((remove-set S) e2 ((list->set S) (list e3 e2 e1)))
                ((list->set S) (list e1 e3)))
  (check-equal? ((remove-set S) e1 ((list->set S) (list e4 e3 e2)))
                ((list->set S) (list e4 e3 e2)))

  (check-true  ((element-set? S) e3 ((list->set S) (list e3 e2 e1))))
  (check-false ((element-set? S) e3 ((list->set S) (list e2 e1))))
  (check-false ((element-set? S) e1 ((list->set S) (list e4 e2 e3))) #f)

  (check-equal? ((intersect-sets S) ((list->set S) (list e3 e2 e1)) ((list->set S) (list e2 e1 e4)))
                ((list->set S) (list e2 e1)))
  
  (check-equal? ((union-sets S) ((list->set S) (list e3 e2 e1)) ((list->set S) (list e2 e1 e4)))
                ((list->set S) (list e1 e2 e3 e4)))

  (check-equal? ((difference-sets S) ((list->set S) (list e3 e2 e1)) ((list->set S) (list e2 e1 e4)))
                ((list->set S) (list e3)))
  
  (check-equal? ((subset-sets? S) ((list->set S) (list e3 e2 e1)) ((list->set S) (list e2 e1 e4)))
                #f)
  (check-equal? ((subset-sets? S) ((list->set S) (list e2 e1)) ((list->set S) (list e2 e1 e4)))
                #t))

(define the-tests
  (test-suite
   "general/sets"
   (test-case
    ""
    (check-equal? ((set->list symbols)
                   ((union-sets symbols)
                    ((list->set symbols) '(a c e))
                    ((list->set symbols) '(d e f))))
                  '(a c d e f)))
   ;*************************************************
   (test-case
    "set-package symbols"
    (set-package-test symbols 'w 'x 'y 'z))
   (test-case
    "set-package real-numbers"
    (set-package-test real-numbers 1 2 3 7))
   (test-case
    "set-package numbers"
    (set-package-test numbers +i 2-3i 2+i 1)
    (define S0 (empty-set numbers))
    (define S1 ((adjoin-set numbers) 1 S0))
    (define S2 ((adjoin-set numbers) 1 S1))
    (define S3 ((adjoin-set numbers) 2 S2))
    (define S4 ((adjoin-set numbers) 1.5 S3))
    (check-equal? ((list->set numbers) S4) '(1 1.5 2)))
   (test-case
    "set-package from strings"
    (set-package-test (make-sets-package string-ci=? string-ci<?) "a" "B" "c" "D")
    (set-package-test (make-sets-package string=? string<?) "B" "D" "a" "c"))
   
   ;; sets as list tested with simple:equal? (unordered)
   (test-case
    "list-adjoin"
    (check-equal? (list-adjoin 'x '(1)) '(x 1))
    (check-equal? (list-adjoin '1 '(x 1 z)) '(x 1 z)))
   (test-case
    "list-union"
    (check-equal? (list-union '(1 2 3) '(x y z)) '(1 2 3 x y z))
    (check-equal? (list-union '(1 2 z) '(x y z)) '(1 2 x y z)))
   (test-case
    "list-intersection"
    (check-equal? (list-intersection '(1 2 3) '(x y z)) '())
    (check-equal? (list-intersection '(1 2 z) '(z y 1)) '(1 z)))
   (test-case
    "list-difference"
    (check-equal? (list-difference '(1 2 3) '(x y z)) '(1 2 3))
    (check-equal? (list-difference '(1 2 z) '(z y 1)) '(2)))
   (test-case
    "duplications"
    (check-equal? (duplications? '(1 2 3 4 5 6 3)) #t)
    (check-equal? (duplications? '(1 2 3 4 5 6 7)) #f))
   (test-case
    "remove-duplicates"
    (check-equal? (remove-duplicates '(1 2 3 4 5 6 3)) '(1 2 4 5 6 3)))
   (test-case
    "subset?"
    (check-equal? (subset? '(1 2 z) '(z y 1)) #f)
    (check-equal? (subset? '(z y 1) '(1 2 z)) #f)
    (check-equal? (subset? '(z 1) '(1 2 z)) #t))
   (test-case
    "same-set?"
    (check-equal? (same-set? '(1 2 3 2 1 3) '(3 2 1)) #t)
    (check-equal? (same-set? '(1 2 3 2 1 3 4) '(3 2 1)) #f))
   
   ;; eq-set: elements are compared with eq? and result is eq? to input if nothing changed
   (test-case
    "eq-set/make-empty & empty?"
    (check-equal? (eq-set/empty? (eq-set/make-empty)) #t))
   (test-case
    "eq-set/member? & eq-set/adjoin"
    (check-not-false (eq-set/member? 1 (eq-set/adjoin 1 (eq-set/make-empty))))
    (check-false     (eq-set/member? 0 (eq-set/adjoin 1 (eq-set/make-empty))))
    (define S (eq-set/adjoin 3 (eq-set/adjoin 2 (eq-set/adjoin 5 (eq-set/make-empty)))))
    (check-true (eq? S (eq-set/adjoin 2 S))))
   (test-case
    "eq-set/equal?"
    (check-true (eq-set/equal? (eq-set/adjoin 2 (eq-set/adjoin 5 (eq-set/make-empty)))
                               (eq-set/adjoin 5 (eq-set/adjoin 2 (eq-set/make-empty)))))
    (define S (eq-set/adjoin 3 (eq-set/adjoin 2 (eq-set/adjoin 5 (eq-set/make-empty)))))
    (check-true (eq-set/equal? S (eq-set/adjoin 3 (eq-set/adjoin 2 (eq-set/adjoin 5 (eq-set/make-empty))))))
    (check-true (eq-set/equal? S (eq-set/adjoin 5 (eq-set/adjoin 2 (eq-set/adjoin 3 (eq-set/make-empty))))))
    (check-true (eq-set/equal? S (eq-set/adjoin 2 (eq-set/adjoin 3 (eq-set/adjoin 5 (eq-set/make-empty)))))))
   (test-case
    "eq-set/remove"
    (check-true (eq-set/empty? (eq-set/remove 1 (eq-set/adjoin 1 (eq-set/make-empty)))))
    (define S (eq-set/adjoin 3 (eq-set/adjoin 2 (eq-set/adjoin 5 (eq-set/make-empty)))))
    (check-true (eq? S (eq-set/remove 6 S)))
    (check-false (eq-set/member? 2 (eq-set/remove 2 S))))
   (test-case
    "eq-set/union"
    (define S1 (eq-set/adjoin 3 (eq-set/adjoin 5 (eq-set/make-empty))))
    (define S2 (eq-set/adjoin 3 (eq-set/adjoin 2 (eq-set/adjoin 5 (eq-set/make-empty)))))
    (define S3 (eq-set/adjoin 3 (eq-set/adjoin 6 (eq-set/adjoin 5 (eq-set/make-empty)))))
    (check-false (eq? S1 S2))
    (check-true (eq? (eq-set/union S1 S2) S2))
    (check-true (eq? (eq-set/union S2 S1) S2))
    (check-true (eq-set/equal? (eq-set/union S2 S3) (eq-set/adjoin 6 S2))))
   (test-case
    "eq-set/intersection"
    (define S1 (eq-set/adjoin 3 (eq-set/adjoin 5 (eq-set/make-empty))))
    (define S2 (eq-set/adjoin 3 (eq-set/adjoin 2 (eq-set/adjoin 5 (eq-set/make-empty)))))
    (define S3 (eq-set/adjoin 3 (eq-set/adjoin 6 (eq-set/adjoin 5 (eq-set/make-empty)))))
    (check-true (eq-set/equal? (eq-set/intersection S1 S2) S1))
    (check-true (eq-set/equal? (eq-set/intersection S2 S1) S1))
    (check-true (eq-set/equal? (eq-set/intersection S2 S3) S1)))
   (test-case
    "eq-set/difference"
    (define S1 (eq-set/adjoin 3 (eq-set/adjoin 5 (eq-set/make-empty))))
    (define S2 (eq-set/adjoin 3 (eq-set/adjoin 2 (eq-set/adjoin 5 (eq-set/make-empty)))))
    (define S3 (eq-set/adjoin 3 (eq-set/adjoin 6 (eq-set/adjoin 5 (eq-set/make-empty)))))
    (check-true (eq-set/equal? (eq-set/difference S1 S2) (eq-set/make-empty)))
    (check-true (eq? (eq-set/difference S1 (eq-set/make-empty)) S1))
    (check-true (eq-set/equal? (eq-set/difference S2 S1) (eq-set/adjoin 2 (eq-set/make-empty))))
    (check-true (eq-set/equal? (eq-set/difference S3 S2) (eq-set/adjoin 6 (eq-set/make-empty)))))
   (test-case
    "eq-set/subset?"
    (define S1 (eq-set/adjoin 3 (eq-set/adjoin 5 (eq-set/make-empty))))
    (define S2 (eq-set/adjoin 3 (eq-set/adjoin 2 (eq-set/adjoin 5 (eq-set/make-empty)))))
    (define S3 (eq-set/adjoin 3 (eq-set/adjoin 6 (eq-set/adjoin 5 (eq-set/make-empty)))))
    (check-true (eq-set/subset? S1 S2))
    (check-false (eq-set/subset? S2 S1))
    (check-true (eq-set/subset? S1 S3))
    (check-false (eq-set/subset? S2 S3)))
   
   ;; multi-set
   (test-case
    "multi-set/empty & multi-set/empty?"
    (check-equal? (multi-set/empty? (multi-set/empty)) #t))
   (test-case
    "multi-set/element? & multi-set/adjoin"
    (check-not-false (multi-set/element? 1 (multi-set/adjoin 1 (eq-set/make-empty))))
    (check-false     (multi-set/element? 0 (multi-set/adjoin 1 (eq-set/make-empty))))
    (define S (multi-set/adjoin 3 (multi-set/adjoin 2 (multi-set/adjoin 5 (eq-set/make-empty)))))
    (check-false (equal? S (multi-set/adjoin 2 S))))
   (test-case
    "multi-set/first"
    (define S (multi-set/adjoin 5 (multi-set/adjoin 2 (multi-set/adjoin 5 (eq-set/make-empty)))))
    (check-equal? (multi-set/first S) 5))
   (test-case
    "multi-set/rest"
    (define S (multi-set/adjoin 5 (multi-set/adjoin 2 (multi-set/adjoin 5 (eq-set/make-empty)))))
    (check-equal? (multi-set/first (multi-set/rest S)) 2)
    (check-equal? (multi-set/first (multi-set/rest (multi-set/rest S))) 5)
    (check-true (multi-set/empty? (multi-set/rest (multi-set/rest (multi-set/rest S))))))
   (test-case
    "multi-set/remove"
    (define S (multi-set/adjoin 5 (multi-set/adjoin 2 (multi-set/adjoin 5 (eq-set/make-empty)))))
    (check-equal? (multi-set/remove 5 S) (multi-set/rest S))
    (check-equal? (multi-set/remove 2 S) (multi-set/adjoin 5 (multi-set/rest (multi-set/rest S)))))
   (test-case
    "multi-set/union"
    (define S1 (multi-set/adjoin 3 (multi-set/adjoin 5 (multi-set/empty))))
    (define S2 (multi-set/adjoin 3 (multi-set/adjoin 2 (multi-set/adjoin 5 (multi-set/empty)))))
    (define S3 (multi-set/adjoin 3 (multi-set/adjoin 6 (multi-set/adjoin 5 (multi-set/empty)))))
    (check-equal? (multi-set/union S1 S2) (multi-set/adjoin 5 (multi-set/adjoin 3 S2)))
    (check-equal? (multi-set/union S2 S3) (multi-set/adjoin 5 (multi-set/adjoin 2 (multi-set/adjoin 3 S3)))))
   (test-case
    "multi-set/intersection"
    (define S1 (multi-set/adjoin 3 (multi-set/adjoin 5 (multi-set/empty))))
    (define S2 (multi-set/adjoin 3 (multi-set/adjoin 2 (multi-set/adjoin 5 (multi-set/empty)))))
    (define S3 (multi-set/adjoin 3 (multi-set/adjoin 6 (multi-set/adjoin 5 (multi-set/empty)))))
    (check-equal? (multi-set/intersection S1 S2) (multi-set/adjoin 5 (multi-set/adjoin 3 (multi-set/empty))))
    (check-equal? (multi-set/intersection S2 S1) (multi-set/adjoin 5 (multi-set/adjoin 3 (multi-set/empty))))
    (check-equal? (multi-set/intersection S2 S3) (multi-set/adjoin 5 (multi-set/adjoin 3 (multi-set/empty)))))
   (test-case
    "multi-set/difference"
    (define S1 (multi-set/adjoin 3 (multi-set/adjoin 5 (multi-set/empty))))
    (define S2 (multi-set/adjoin 3 (multi-set/adjoin 2 (multi-set/adjoin 5 (multi-set/empty)))))
    (define S3 (multi-set/adjoin 3 (multi-set/adjoin 6 (multi-set/adjoin 5 (multi-set/empty)))))
    (check-equal? (multi-set/difference S1 S2) (multi-set/empty))
    (check-equal? (multi-set/difference S1 (multi-set/empty)) (multi-set/adjoin 5 (multi-set/adjoin 3 (multi-set/empty))))
    (check-equal? (multi-set/difference S2 S1) (multi-set/adjoin 2 (multi-set/empty)))
    (check-equal? (multi-set/difference S3 S2) (multi-set/adjoin 6 (multi-set/empty))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))