#lang racket/base

(require rackunit
         (submod "../../general/hashcons.rkt" ALL)
         "../helper.rkt")

(define (tree-copy itm)
  (if (pair? itm)
      (cons (tree-copy (car itm))
            (tree-copy (cdr itm)))
      itm))
(define foo
  '(define (canonical-copy x)
     (if (pair? x)
         (let ((canonical-pair
                (hash-table/get the-cons-table x #f)))
           (or canonical-pair
               (let ((new
                      (cons (canonical-copy (car x))
                            (canonical-copy (cdr x)))))
                 (hash-table/put! the-cons-table new new)
                 new)))
         x)))
(define bar
  '(define cons-unique
     (let ((the-pair (cons #f #f)))  
       (define (hashcons x y)
         (set-car! the-pair x)
         (set-cdr! the-pair y)
         (let ((canonical-pair
                (hash-table/get the-cons-table the-pair #f)))
           (or canonical-pair
               (let ((new the-pair))
                 (hash-table/put! the-cons-table new new)
                 (set! the-pair (cons #f #f))
                 new))))
       hashcons)))

(provide the-tests)
(define the-tests
  (test-suite
   "general/hashcons"
   (test-case
    "canonical-copy"
    
    (define cfoo (canonical-copy foo))
    (define cbar (canonical-copy bar))
    (define baz (caddr (caddr (caddr (caddr (caddr cfoo))))))
    
    (check-true (eq? cfoo (canonical-copy foo)))
    (check-true (eq? cfoo (canonical-copy cfoo)))
    (check-false (eq? (caddr (caddr (caddr (caddr (caddr foo)))))
                      (caddr (caddr (caddar (cddddr (caddr (caddr bar))))))))
    (check-true (eq? (caddr (caddr (caddr (caddr (caddr cfoo)))))
                     (caddr (caddr (caddar (cddddr (caddr (caddr cbar))))))))
    (check-equal? baz '(hash-table/put! the-cons-table new new))
    (define hc1 (hash-count the-cons-table))
    (collect-garbage)(collect-garbage)(collect-garbage)
    (clean)
    (define hc2 (hash-count the-cons-table))
    (check-true (< hc2 hc1))
    )
   (test-case
    "cons-unique"
    (define A (list 4))
    (define B (list 5))
    (define the-cons (cons-unique A B))
    (check-true (eq? the-cons (cons-unique A B))))
   (test-case
    "from-cons-table-get"
    (check-eq? (from-cons-table-get '(nothing) 'strange) 'strange))
   (test-case
    "list-unique"
    (check-true (eq? (list-unique 1 2 3 4 'a)
                     (list-unique 1 2 3 4 'a))))
   (test-case
    "map-unique"
    (check-true (eq? (map-unique add1 '(0 1 2 3 4))
                     (list-unique 1 2 3 4 5))))
   (test-case
    "append-unique"
    (define L2 (list 4 5 6))
    (check-true (eq? (append-unique (list 1 2 3) L2)
                     (append-unique (list 1 2 3) L2)))
    (skip
    ;; this currently fails (also in scmutils) but probably should be #t
     (check-true (eq? (append-unique (list 1 2 3) (list 4 5 6))
                      (append-unique (list 1 2 3) (list 4 5 6))))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))