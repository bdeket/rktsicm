#lang racket/base

(require rackunit
         racket/list
         "../../general/hashcons.rkt")

(define the-tests
  (test-suite
   "general/hashcons"
   (test-case
    ""
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
    (define cfoo (canonical-copy foo))
    (define cbar (canonical-copy bar))
    (define baz (caddr (caddr (caddr (caddr (caddr cfoo))))))
    
    (check-true (eq? cfoo (canonical-copy foo)))
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
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))