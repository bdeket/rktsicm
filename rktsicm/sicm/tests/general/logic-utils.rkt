#lang racket/base

(require rackunit
         "../../general/logic-utils.rkt"
         "../../general/eq-properties.rkt"
         "../../general/notes.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "general/logic-utils"
   ;; assume!
   (test-case
    "&or *or"
    (check-false (&or '()))
    (check-true  (&or '(#f #t)))
    (check-false (&or '(#f #f)))
    (check-true  (*or #f #f #t))
    (check-false (*or #f #f #f)))
   (test-case
    "&and *and"
    (check-true  (&and '()))
    (check-true  (&and '(#t #t)))
    (check-false (&and '(#t #f)))
    (check-true  (*and #t #t #t))
    (check-false (*and #t #t #f)))
   (test-case
    "true? / false?"
    (check-true (eq? (false? #f)  #t))
    (check-true (eq? (false? '()) #f))
    (check-true (eq? (false? #t)  #f))
    (check-true (eq? (true? #f)   #f))
    (check-true (eq? (true? '())  #f))
    (check-true (eq? (true? #t)   #t)))
   (test-case
    "conjunction"
    (define pos-num? (conjunction real? (λ (x) (< 0 x))))
    (check-true  (pos-num? 1))
    (check-true  (pos-num? 1e300))
    (check-false (pos-num? 'a))
    (check-false (pos-num? 0)))
   (test-case
    "disjunction"
    (define r-or-s? (disjunction real? symbol?))
    (check-true  (r-or-s? 1))
    (check-true  (r-or-s? 'a))
    (check-false (r-or-s? "a"))
    (check-false (r-or-s? '(list))))
   (test-case
    "negation"
    (define not-r? (negation real?))
    (check-true  (not-r? "1"))
    (check-true  (not-r? 'a))
    (check-false (not-r? 1))
    (check-false (not-r? 1e300)))
   (test-case
    "implication"
    (define not-r? (implication real? (λ (x) (< 0 x))))
    ;; either not a real? , or a real > 0
    (check-true  (not-r? "1"))
    (check-true  (not-r? 'a))
    (check-true  (not-r? 1))
    (check-false (not-r? -1)))
   (test-case
    "assume!"
    ;;numeric args
    ;; True assumptions: will not be noted
    (check-equal? (assume! '(real? 1) 'R1) 'OK) ;; kernel is not loaded so only use racket/base
                                                ;; defined items
    (check-equal? (assume! '(= 1 1.000000000000001) 'R2) 'OK)
    (check-equal? (assume! #t 'R3) 'OK)
    (check-equal? *notes* '())
    (check-equal? (hash-count eq-properties) 0)

    ;; False assumptions: execute if-false => no note
    (define G (gensym))
    (check-equal? (assume! '(real? 4+2i) 'R4 (λ () G)) G)
    (check-equal? (assume! '(= 1 2) 'R5 (λ () G)) G)
    (check-equal? (assume! #f 'R6 (λ () G)) G)
    (check-equal? *notes* '())
    (check-equal? (hash-count eq-properties) 0)

    ;; bad (non procedure?) operator
    (check-exn exn:fail? (λ () (assume! '(or 1 2 3) 'R2)))

    ;; Assumptions that can't be tested
    (define (test-noted prep resp)
      (check-equal? (assume! prep resp) 'noted)
      (define note (car *notes*))
      (check-equal? `(assuming ,prep) note)
      (check-equal? (eq-get note 'rules) (list resp))
      (clear-notes!))
    ;; unknown function
    (test-noted '(unknownfunction? 1 2 3) 'R7)
    ;; non-number args
    (test-noted '(real? a) 'R8)
    ;; any
    (test-noted 'halo 'R9)
    
    ;; default false assumption
    (let ([prep '(positive? -1)])
      (check-equal? (assume! prep 'R10) 'noted)
      (define note (car *notes*))
      (check-equal? `(assuming (false! ,prep)) note)
      (check-equal? (eq-get note 'rules) '(R10))
      (clear-notes!))
    
    )
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))