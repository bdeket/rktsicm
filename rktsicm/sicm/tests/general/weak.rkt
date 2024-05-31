#lang racket/base

(require rackunit
         "../../general/weak.rkt")

(define the-tests
  (test-suite
   "general/weak"
   (test-case
    "weak-pair"
    ;; fixnums are never GC'ed
    (define W0 (weak-cons 1 2))
    (check-equal? (weak-car W0) 1)
    (check-true (weak-pair/car? W0))
    (check-equal? (weak-cdr W0) 2)
    
    (check-true (weak-pair? W0))
    
    ;; force GC
    (define W1 (weak-cons (list 1.5) (list 5.8)))
    (weak-set-cdr! W1 (list 6.9))
    (collect-garbage)(collect-garbage)(collect-garbage)
    (check-true (weak-pair? W1))
    (check-false (weak-pair/car? W1))
    (check-true (gc-reclaimed-object? (weak-car W1)))
    (check-equal? (weak-cdr W1) '(6.9))
    )
   (test-case
    "weak-list"
    (define W0 (list->weak-list (build-list 2 values)))
    (check-equal? (weak-car W0) 0)
    (check-equal? (weak-car (weak-cdr W0)) 1)
    (check-equal? (weak-cdr (weak-cdr W0)) '())
    (check-true (weak-list-intact? W0))
    (check-equal? (weak-length W0) 2)

    (define W1 (list->weak-list (list 1 2 (list 3) 4)))
    (collect-garbage)(collect-garbage)(collect-garbage)
    (check-equal? (weak-car W1) 1)
    (check-equal? (weak-car (weak-cdr W1)) 2)
    (check-false (weak-pair/car? (weak-cdr (weak-cdr W1))))
    (check-false (weak-list-intact? W1))
    (check-equal? (weak-length W1) 4)
    )
   (test-case
    "get-weak-member"
    (define W (list->weak-list (cons (list 5.8) (build-list 10 values))))
    (check-equal? (get-weak-member 4 W) 4)
    (check-false (get-weak-member 100 W))
    (collect-garbage)(collect-garbage)(collect-garbage)
    (check-false (get-weak-member '(5.8) W)))
   (test-case
    "weak-find"
    (define W (cons (weak-cons (list 5.8) 0)
                    (build-list 10 (λ (i) (weak-cons i (* i i))))))
    (check-equal? (weak-find 2 W) 2)
    (check-false (weak-find -1 W))
    (collect-garbage)(collect-garbage)(collect-garbage)
    (check-false (weak-find '(5.8) W)))
   (test-case
    "weak-finder"
    weak-finder
    weak-find-equal?
    weak-find-eqv?
    weak-find-eq?

    (define W (list 5
                    (cons 3 2)
                    #f
                    (weak-cons 6 4)
                    (weak-cons (list 5.8) 0)
                    '(9 . 8)))
    (check-false (weak-find-equal? 5 W))
    (check-equal? (weak-find-eq? 3 W) 2)
    (check-equal? (weak-find-eqv? 6 W) 4)
    (collect-garbage)(collect-garbage)(collect-garbage)
    (check-false (weak-find-equal? '(5.8) W))
    (check-false (weak-find-equal? 9 W))
    )
   (test-case
    "purge-list"
    (define W (list (cons 3 2)
                    (weak-cons 6 4)
                    (weak-cons (list 5.8) 0)
                    '(9 . 8)))
    (collect-garbage)(collect-garbage)(collect-garbage)
    (check-equal? (length (purge-list W 0)) 3)
    (check-equal? (length (purge-list W 2)) 2))
   (test-case
    "clean-weak-list"
    (define W (list->weak-list (list 1 2 (list 5.8) 6)))
    (collect-garbage)(collect-garbage)(collect-garbage)
    (define V (clean-weak-list W))
    (check-equal? (weak-length V) 3)
    ;;!! W is modified !!
    (check-equal? (weak-car (weak-cdr (weak-cdr W))) 6))
   (test-case
    "clean-weak-alist"
    ;; clean-sutable-alist & clean-alist
    ;; clean-weak-list is not mutating as in original!
    (define W (list (weak-cons 1 2)
                    (weak-cons 3 4)
                    (weak-cons (list 5.8) 6)
                    (weak-cons 7 8)))
    (collect-garbage)(collect-garbage)(collect-garbage)
    (define V (clean-weak-alist W))
    (check-equal? (length V) 3)
    )
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))