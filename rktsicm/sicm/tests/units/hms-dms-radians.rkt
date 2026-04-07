#lang racket/base

(require rackunit
         "../../kernel/numeric.rkt"
         "../../units/hms-dms-radians.rkt"
         "../helper.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "units/hms-dms-radians"
   (test-case
    "degrees<->radians"
    (check-within (degrees->radians 180) :pi 1e-15)
    (check-within (radians->degrees :pi) 180 1e-15)
    (for ([i (in-range 10)])
      (define x (- (* (random) 10) 5))
      (check-within (degrees->radians (radians->degrees x)) x 1e-15)))
   (test-case
    "xms<->x"
    (check-equal? (xms->x '(45 12 36)) 4521/100)
    (check-equal? (x->xms 4521/100) '(45 12 36))
    (for ([i (in-range 10)])
      (define xms (list (random 500) (random 60) (random 60)))
      (check-equal? (x->xms (xms->x xms)) xms)))
   (test-case
    "dms<->d"
    (check-equal? (dms->d '(45 12 36)) 4521/100)
    (check-equal? (d->dms 4521/100) '(45 12 36))
    (for ([i (in-range 10)])
      (define dms (list (random 500) (random 60) (random 60)))
      (check-equal? (d->dms (dms->d dms)) dms)))
   (test-case
    "dms<->radians"
    (check-within (dms->radians '(45 12 36)) 0.7890633548266364 1e-15)
    (check-within (radians->dms 0.7890633548266364) '(45 12 36) 1e-10)
    (for ([i (in-range 10)])
      (define d (* 500 (random)))
      (check-within (dms->radians (radians->dms d)) d 1e-10)))
   (test-case
    "hours<->radians"
    (check-within (hours->radians 25.5) 6.67588438887831 1e-15)
    (check-within (radians->hours 6.67588438887831) 25.5 1e-13)
    (for ([i (in-range 10)])
      (define h (* (random) 60))
      (check-within (radians->hours (hours->radians h)) h 1e-13)))
   (test-case
    "hms<->h"
    (check-equal? (hms->h '(45 12 36)) 4521/100)
    (check-equal? (h->hms 4521/100) '(45 12 36))
    (for ([i (in-range 10)])
      (define hms (list (random 500) (random 60) (random 60)))
      (check-equal? (h->hms (hms->h hms)) hms)))
   (test-case
    "hms<->radians"
    (check-within (hms->radians '(45 12 36)) 11.835950322399546 1e-15)
    (check-within (radians->hms 11.835950322399546) '(45 12 36) 1e-10)
    (check-within (radians->hms (dms->radians '(5 0 0))) '(0 20 0) 1e-10)
    (check-within (radians->hms (dms->radians '(15 0 0))) '(1 0 0) 1e-10)
    (for ([i (in-range 10)])
      (define h (* 500 (random)))
      (check-within (hms->radians (radians->hms h)) h 1e-10)))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))