#lang racket/base

(require rackunit
         racket/list
         (submod "../../general/memoize.rkt" ALL))

(define (fib1 n)
  (if (< n 2)
      n
      (+ (fib1 (- n 1)) (fib1 (- n 2)))))
(define (fib2 n m)
  (if (< n 2)
          (if (< m 2)
              (+ n m)
              (+ (fib2 n (- m 1))
                 (fib2 n (- m 2))))
          (+ (fib2 (- n 1) m)
             (fib2 (- n 2) m))))

(define (LLEN x)
  (cond
    [(hash? x) (hash-count x)]
    [(list? x) (length x)]
    [(procedure? x) (let lp ([v (hash-values (1d-table-v (x 'table)))]
                             [s 0])
                      (cond
                        [(null? v) s]
                        [(1d-table? (car v))
                         (lp (cdr v)
                             (lp (hash-values (1d-table-v (car v))) s))]
                        [else (lp (cdr v) (+ s 1))]))]
    [else x]))

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
    (check-equal? (take (foo 'statistics) 2) '(34 36)))
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
    (check-equal? (take (foo 'statistics) 2) '(48 55)))
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
   ;***********************************************
   (test-case
    "fib1"
    (check-equal? (fib1 20) 6765)
    (check-equal? (fib1 35) 9227465))
   (test-case
    "the-memoizers"
    (check-equal? (length (the-memoizers)) (hash-count (cdr *memoizers*))))
   (test-case
    "hash-memoize-1arg"
    (let* ([fib1 (procedure-rename fib1 'fib1)]; don't leave links to memoize proc so it can be gc'd
           [mfib (hash-memoize-1arg fib1)])
      (define MEM (hash-ref (cdr *memoizers*) mfib))
      (check-true (eq? mfib (memoizer-fun MEM)))
      (define info (memoizer-info MEM))
      (define reset (memoizer-reset MEM))
      (check-true (eq? fib1 (memoizer-org MEM)))
      (check-equal? (procedure-arity fib1) (procedure-arity mfib))
      (define (checker in out hit mis size)
        (check-equal? (mfib in) out)
        (define INFO (info))
        (check-equal? (car INFO) hit)
        (check-equal? (cadr INFO) mis)
        (check-equal? (LLEN (cadr INFO)) size))
      (checker  3   2 0 1 1)
      (checker  3   2 1 1 1)
      (checker  3   2 2 1 1)
      (checker  9  34 2 2 2)
      (checker 12 144 2 3 3)
      (reset)
      (checker  3   2 0 1 1)))
   (test-case
    "linear-memoize"
    (let* ([fib1 (procedure-rename fib1 'fib1)]; don't leave links to memoize proc so it can be gc'd
           [mfib (linear-memoize fib1)])
      (define MEM (hash-ref (cdr *memoizers*) mfib))
      (check-true (eq? mfib (memoizer-fun MEM)))
      (define info (memoizer-info MEM))
      (define reset (memoizer-reset MEM))
      (check-true (eq? fib1 (memoizer-org MEM)))
      (check-equal? (procedure-arity fib1) (procedure-arity mfib))
      (define (checker in out hit mis size)
        (check-equal? (mfib in) out)
        (define INFO (info))
        (check-equal? (car INFO) hit)
        (check-equal? (cadr INFO) mis)
        (check-equal? (LLEN (cadr INFO)) size))
      (checker  3   2 0 1 1)
      (checker  3   2 1 1 1)
      (checker  3   2 2 1 1)
      (checker  9  34 2 2 2)
      (checker 12 144 2 3 3)
      (reset)
      (checker  3   2 0 1 1)))
   (test-case
    "samritchie-memoizer"
    (let* ([fib1 (procedure-rename fib1 'fib1)]; don't leave links to memoize proc so it can be gc'd
           [mfib (samritchie-memoizer fib1)])
      (define MEM (hash-ref (cdr *memoizers*) mfib))
      (check-true (eq? mfib (memoizer-fun MEM)))
      (define info (memoizer-info MEM))
      (define reset (memoizer-reset MEM))
      (check-true (eq? fib1 (memoizer-org MEM)))
      (check-equal? (procedure-arity fib1) (procedure-arity mfib))
      (define (checker in out hit mis size)
        (check-equal? (mfib in) out)
        (define INFO (info))
        (check-equal? (car INFO) hit)
        (check-equal? (cadr INFO) mis)
        (check-equal? (LLEN (cadr INFO)) size))
      (checker  3   2 0 1 1)
      (checker  3   2 1 1 1)
      (checker  3   2 2 1 1)
      (checker  9  34 2 2 2)
      (checker 12 144 2 3 3)
      (reset)
      (checker  3   2 0 1 1)
      (check-true (eq? mfib (samritchie-memoizer fib1)))))
   (test-case
    "clear-memoizer-tables"
    ;; if this test is run in the same proces as others we can not be sure *memoizers* is empty...
    (collect-garbage 'major)
    (clear-memoizer-tables)
    (define (∑)
      (for/sum ([(k v) (in-hash (cdr *memoizers*))])
        ;(println (list k ((memoizer-info v)) (LLEN (caddr ((memoizer-info v))))))
        (LLEN (caddr ((memoizer-info v))))))
    (define ∑0 (∑))
    (let* ([fib1 (procedure-rename fib1 'fib1)]; don't leave links to memoize proc so it can be gc'd
           [fib2 (procedure-rename fib2 'fib2)]
           [mfib-s1 (samritchie-memoizer fib1)]
           [mfib-s2 (samritchie-memoizer fib1)]
           [mfib-s3 (samritchie-memoizer fib2)]
           [mfib-h1 (hash-memoize-1arg fib1)]
           [mfib-l1 (linear-memoize fib1)]
           [mfib-l2 (linear-memoize fib2)])
      (check-eq? mfib-s1 mfib-s2)
      (check-not-eq? mfib-s1 mfib-s3)
      (mfib-s1 1) (mfib-s2 2) (mfib-s3 3 4) (mfib-h1 5) (mfib-l1 6) (mfib-l2 7 8)
      (check-equal? (∑) (+ ∑0 6))
      (clear-memoizer-tables)
      (check-equal? (∑) ∑0)))
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