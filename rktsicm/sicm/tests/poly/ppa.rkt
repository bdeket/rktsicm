#lang racket/base

(require rackunit
         "../../poly/ppa.rkt"
         "../../simplify/pcf.rkt")

(provide the-tests)

(define P0 (ppa-make-from-poly 0 1 (poly:dense-> '(1))))
(define P1 (ppa-make-from-poly 1 3 (poly:dense-> '(2 1))))
(define P2 (ppa-make-from-poly 3 6 (poly:dense-> '(3/2 -1))))
(define the-tests
  (test-suite
   "poly/ppa"
   (test-case
    "ppa-make-from-poly"
    (check-equal? (ppa-low-bound P0) 0)
    (check-equal? (ppa-high-bound P2) 6)
    (check-equal? (ppa-poly (ppa-body P0)) 1)
    (check-true (ppa-terminal? (ppa-body P0))))
   (test-case
    "ppa-adjoin"
    (define P (ppa-adjoin P0 P1))
    (define Q (ppa-adjoin P P2))
    (check-true (ppa-split? (ppa-body P)))
    (check-false (ppa-split? (ppa-body P0)))
    (check-false (ppa-terminal? (ppa-body P)))
    (check-equal? (ppa-split (ppa-body P)) 1)
    (check-equal? (ppa-split (ppa-body Q)) 3)
    (check-equal? (ppa-low-side (ppa-body P)) (ppa-body P0))
    (check-equal? (ppa-high-side (ppa-body P)) (ppa-body P1))
    (check-exn #px"PPAs not adjacent -- PPA-ADJOIN" (λ () (ppa-adjoin P0 P2))))
   (test-case
    "ppa-value"
    (define P (ppa-adjoin (ppa-adjoin P0 P1) P2))
    (check-equal? (ppa-value P0 0.5) 1)
    (check-equal? (ppa-value P1 2) 2)
    (check-equal? (ppa-value P2 4) 2)
    ;; ppa is defined around middle of interval:
    (check-equal? (ppa-value (ppa-make-from-poly 0 1 (poly:dense-> '(0 1 -1))) 1/2) 0)
    (check-equal? (ppa-value (ppa-make-from-poly 10 11 (poly:dense-> '(0 1 -1))) 21/2) 0)
    ;; and not outside
    (check-exn #px"Out of bounds -- PPA-VALUE" (λ () (ppa-value P0 2)))
    (check-exn #px"Out of bounds -- PPA-VALUE" (λ () (ppa-value P1 4)))
    (check-equal? (ppa-value P 1/2) 1)
    (check-equal? (ppa-value P 2) 2)
    (check-equal? (ppa-value P 4) 2)
    ;; boundary is part of next interval
    (check-equal? (ppa-value (ppa-adjoin (ppa-make-from-poly 0 1 1) (ppa-make-from-poly 1 2 2)) 1) 2)
    ;; garbled ppa (should not be possible to create)
    (check-exn #px"Bad body -- PPA-SEARCH" (λ () (ppa-value '((0 . 3) bad 1) 1)))
    (check-equal? (ppa-max-degree P) 1)
    (check-equal? (ppa-max-degree (ppa-adjoin P0 (ppa-make-from-poly 1 2 (poly:dense-> '(0 1 -1 2 3 4))))) 5)
    (check-equal? (ppa-size P) 3)
    (check-equal? (ppa-size P0) 1))
   (test-case
    "make-ppa"
    (define eps .1)
    (define P (make-ppa sin -4 4 3 eps))
    (check-equal? (ppa-low-bound P) -4)
    (check-equal? (ppa-high-bound P) 4)
    ;; accuracy is reached with higher degree or more pieces...
    (check-true (<= (ppa-max-degree P) 3))
    (check-true (<= (ppa-max-degree (make-ppa sin -4 4 4 .1)) 4))
    (for ([i (in-range 10)])
      (define x (- (* (random 8)) 4))
      (check-true (<= (abs (- (sin x) (ppa-value P x))) eps))))
   (test-case
    "ppa-memo"
    (define eps .01)
    (define F (ppa-memo sin -4 4 4 eps))
    (for ([i (in-range 10)])
      (define x (- (* (random 8)) 4))
      (check-true (<= (abs (- (sin x) (F x))) eps))))
   (test-case
    "make-smooth-ppa"
    (define eps .01)
    (define P (make-smooth-ppa (list sin cos (λ (x) (- (sin x)))) -4 4 eps))
    (for ([i (in-range 10)])
      (define x (- (* (random 8)) 4))
      (check-true (<= (abs (- (sin x) (ppa-value P x))) eps))))
   (test-case
    "smooth-ppa-memo"
    (define eps .001)
    (define F (smooth-ppa-memo (list sin cos (λ (x) (- (sin x))) (λ (x) (- (cos x)))) -4 4 eps))
    (for ([i (in-range 10)])
      (define x (- (* (random 8)) 4))
      (check-true (<= (abs (- (sin x) (F x))) eps))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))