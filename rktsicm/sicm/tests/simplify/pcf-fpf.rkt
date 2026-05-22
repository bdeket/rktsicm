#lang racket/base

(require rackunit
         (only-in "../../kernel.rkt") ;; load generic functions
         "../../simplify/pcf-fpf.rkt"
         "../../simplify/fpf.rkt"
         "../../simplify/pcf.rkt"
         "../../simplify/sparse.rkt"
         "../../simplify/sparse-gcd.rkt"
         "../helper.rkt")

;; TODO ;; there are too many poly's
;; there is pcf that can be *dense* or *sparse*
;; there is fpf (sparse)
;; there is sparse
;; and there is a0-an ordered *dense* in some algorithms
;; clean up! and keep only 1 dense and 1 sparse type

(provide the-tests)
(define the-tests
  (test-suite
   "simplify/pcf-fpf"
   (test-case
    "pcf<->sparse"
    (check-equal? (pcf->sparse 0) '())
    (check-equal? (pcf->sparse 3) (list (sparse-term '() 3)))
    (check-equal? (pcf->sparse (poly/make 1 '(1 3 1 2)))
                  (list (sparse-term '(3) 1) (sparse-term '(2) 3) (sparse-term '(1) 1) (sparse-term '(0) 2)))
    (check-equal? (pcf->sparse (poly/make 2 (list (poly/make 1 '(3 2 0)) 0)))
                  (list (fpf:make-term '(1 2) 3) (fpf:make-term '(1 1) 2)))

    (check-equal? (sparse->pcf '()) 0)
    (check-equal? (sparse->pcf (list (sparse-term '() 0))) 0)
    (check-equal? (sparse->pcf (list (sparse-term '() 3))) 3)
    (check-equal? (sparse->pcf (list (sparse-term '(0) 3))) 3)
    (check-equal? (sparse->pcf (list (sparse-term '(0 0) 3))) 3)
    (check-equal? (sparse->pcf (list (sparse-term '(3) 1) (sparse-term '(2) 3) (sparse-term '(1) 1) (sparse-term '(0) 2)))
                  (poly/make 1 '(1 3 1 2)))
    (check-equal? (sparse->pcf (list (fpf:make-term '(1 2) 3) (fpf:make-term '(1 1) 2)))
                  (poly/make 2 (list (poly/make 1 '(3 2 0)) 0)))
    (check-exn #px"Bad sparse -- sparse->pcf"
               (λ () (sparse->pcf (list (sparse-term '() 3) (sparse-term '() 4))))))
   (test-case
    "fpf<->pcf"
    (check-equal? (fpf->pcf (fpf:make (list (fpf:make-term '(1 2) 3) (fpf:make-term '(1 1) 2))))
                  (poly/make 2 (list (poly/make 1 '(3 2 0)) 0)))
    (check-equal? (pcf->fpf (poly/make 2 (list (poly/make 1 '(3 2 0)) 0)))
                  (fpf:make (list (fpf:make-term '(1 2) 3) (fpf:make-term '(1 1) 2)))))
   (test-case
    "poly<->sparse"
    (check-equal? (poly->sparse (fpf:make (list (fpf:make-term '(1 2) 3) (fpf:make-term '(1 1) 2))))
                  (list (sparse-term '(1 2) 3) (sparse-term '(1 1) 2)))
    (check-equal? (poly->sparse (poly/make 2 (list (poly/make 1 '(3 2 0)) 0)))
                  (list (sparse-term '(1 2) 3) (sparse-term '(1 1) 2)))
    ;;TODO;; should it accept numbers? (since they are pcf?)
    (check-exn #px"Unknown type: poly->sparse" (λ () (poly->sparse 3)))
    
    (check-equal? (sparse->poly (list (sparse-term '(1 2) 3) (sparse-term '(1 1) 2)) '*fpf*)
                  (fpf:make (list (fpf:make-term '(1 2) 3) (fpf:make-term '(1 1) 2))))
    (check-equal? (sparse->poly (list (sparse-term '(1 2) 3) (sparse-term '(1 1) 2)) '*pcf*)
                  (poly/make 2 (list (poly/make 1 '(3 2 0)) 0)))
    (check-equal? (sparse->poly (list (sparse-term '() 3)) '*pcf*)
                  3)
    (check-exn #px"Unknown type: sparse->poly"
                  (λ () (sparse->poly (list (sparse-term '() 3)) '*number*))))
   (test-case
    "gcd-helpers"
    (check-not-exn (λ () (gcd-check-same-arity (poly/make 2 (list (poly/make 1 '(1 0)) 0))
                                               (poly/make 2 (list (poly/make 1 '(1 0)) 0)))))
    ;; TODO;; only works with pcf:*dense* why
    (check-exn #px"Unequal arities -- poly:gcd"
               (λ () (gcd-check-same-arity (poly/make 3 (list (poly/make 1 '(1 0)) (poly/make 2 '(1 1))))
                                           (poly/make 2 (list (poly/make 1 '(1 0)) 0)))))
    (check-equal? (gcd-target-type (poly/make 1 '(1 2 3))) '*pcf*)
    (check-equal? (gcd-target-type (fpf:make (list (fpf:make-term '(1) 2)))) '*fpf*)
    (check-exn #px"Unknown type: gcd-target-type"
               (λ () (gcd-target-type 3))))
   (test-case
    "poly/gcd-classical"
    (define P0 (fpf:make (list (fpf:make-term '(2 1) 3) (fpf:make-term '(0 2) 1))))
    (define P1 (fpf:make (list (fpf:make-term '(1 0) 1) (fpf:make-term '(0 1) 1))))
    (define P2 (fpf:make (list (fpf:make-term '(2 1) 1) (fpf:make-term '(0 1) 2))))
    (define P3 (fpf:* P0 P2))
    (define P4 (fpf:* P1 P2))
    (check-equal? (poly/gcd-classical (fpf->pcf P3) (fpf->pcf P4)) (fpf->pcf P2))
    (check-equal? (poly/gcd-classical (fpf->pcf P3) P4) (fpf->pcf P2))
    (check-equal? (poly/gcd-classical P3 (fpf->pcf P4)) P2)
    (check-equal? (poly/gcd-classical P3 P4) P2)
    (check-exn #px"What do I do here?"
               (λ () (poly/gcd-classical (fpf->pcf P3) 3)))
    (check-exn #px"What do I do here?"
               (λ () (poly/gcd-classical P3 3)))
    (check-exn #px"What do I do here?"
               (λ () (poly/gcd-classical 3 P2))))

   (test-case
    "poly:gcd"
    (define P0 (fpf:make (list (fpf:make-term '(2 1) 3) (fpf:make-term '(0 2) 6))))
    (define P1 (fpf:make (list (fpf:make-term '(1 0) 1) (fpf:make-term '(0 1) 1))))
    (define P2 (fpf:make (list (fpf:make-term '(2 1) 1) (fpf:make-term '(0 1) 2))))
    (define P3 (fpf:* P0 P2))
    (define P4 (fpf:* P1 P2))
    (check-equal? (poly:gcd 0 P4) P4)
    (check-equal? (poly:gcd P3 0) P3)
    (check-equal? (poly:gcd 1 P4) 1)
    (check-equal? (poly:gcd P3 1) 1)
    (check-equal? (poly:gcd 3 P4) 1)
    (check-equal? (poly:gcd 6 15) 3)
    (check-equal? (poly:gcd P3 3) 3)
    (check-equal? (poly:gcd (fpf->pcf P3) (fpf->pcf P4)) (fpf->pcf P2))
    ;; finicky (depends on how fast it is calculated)
    (let ([P0 '(*sparse* 2 (38 *sparse* 1 (17 . 14)) (37 *sparse* 1 (31 . 4)) (23 *sparse* 1 (33 . 9)) (21 *sparse* 1 (14 . 10)) (8 *sparse* 1 (18 . 19)) (6 *sparse* 1 (17 . -13)) (5 *sparse* 1 (37 . 6)))]
          [P1 '(*sparse* 2 (37 *sparse* 1 (35 . -3)) (31 *sparse* 1 (19 . -12)) (28 *sparse* 1 (10 . -11)) (18 *sparse* 1 (38 . -20)) (7 *sparse* 1 (4 . 4)) (1 *sparse* 1 (34 . 4)))])
      (check-equal? (poly:gcd P0 P1) '(*dense* 2 (*sparse* 1 (4 . 1)) 0)))
    (let ([P0 '(*sparse* 2 (46 *sparse* 1 (9 . -90)) (43 *sparse* 1 (16 . -13)) (33 *sparse* 1 (3 . 126)))]
          [P1 '(*sparse* 2 (32 *dense* 1 -483 0) (27 *sparse* 1 (44 . -49)))])
      (check-equal? (poly:gcd P0 P1) '(*sparse* 2 (27 *dense* 1 1 0))))
    ;; TODO;; find poly's for which the result is #f
    )
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))