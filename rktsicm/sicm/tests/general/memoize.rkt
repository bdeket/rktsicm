#lang racket/base

(require rackunit
         racket/list
         "../../general/memoize.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "general/memoize"
   (test-case
    "fib"
    (define (fib n)
      (if (< n 2)
          n
          (+ (fib (- n 1)) (fib (- n 2)))))
    (check-equal? (fib 20) 6765)
    (check-equal? (fib 35) 9227465)
    (define foo (memoize-multi-arg-eq fib))
    (set! fib (foo 'the-memoized-procedure))
    (check-equal? (fib 20) 6765)
    (check-equal? (fib 35) 9227465)
    (check-equal? (take (foo 'statistics) 2) '(34 36))
    )
   (test-case
    "ffib"
    (define (ffib n m)
      (if (< n 2)
          (if (< m 2)
              (+ n m)
              (+ (ffib n (- m 1))
                 (ffib n (- m 2))))
          (+ (ffib (- n 1) m)
             (ffib (- n 2) m))))
    (check-equal? (time (ffib 18 18)) 21607408)
    (define foo (memoize-multi-arg-eq ffib))
    (set! ffib (foo 'the-memoized-procedure))
    (check-equal? (time (ffib 18 18)) 21607408)
    (check-equal? (take (foo 'statistics) 2) '(48 55))
    )
   (test-case
    "ffib+"
    (define (ffib n m)
      (if (< n 2)
          (if (< m 2)
              (+ n m)
              (+ (ffib n (- m 1))
                 (ffib n (- m 2))))
          (+ (ffib (- n 1) m)
             (ffib (- n 2) m))))
    (set! ffib (simple-memoize-multi-arg-eq ffib))
    (check-equal? (time (ffib 18 18)) 21607408))
   #|

(define (ffib n m)
  (if (< n 2)
      (if (< m 2)
          (+ n m)
          (+ (ffib n (- m 1))
             (ffib n (- m 2))))
      (+ (ffib (- n 1) m)
         (ffib (- n 2) m))))

(show-time (lambda () (ffib 18 18)))
;process time: 15200 (15200 RUN + 0 GC); real time: 15196
;Value: 21607408

(define ffib (simple-memoize-multi-arg-eq ffib))
;Value: ffib

(show-time (lambda () (ffib 18 18)))
;process time: 0 (0 RUN + 0 GC); real time: 2
;Value: 21607408
|#))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))