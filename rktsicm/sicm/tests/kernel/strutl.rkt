#lang racket/base

(require rackunit
         (prefix-in rkt: (only-in racket/stream stream stream->list))
         "../../kernel/strutl.rkt"
         "../helper.rkt"
         )

(provide the-tests)
(define the-tests
  (test-suite
   "kernel/strutl"
   (test-case
    "stream-basics"
    ;; MIT                                 RKT
    ;; (empty-stream? '())      ;=> #t     (stream-empty? '())  ;=> #t
    ;; (null? the-empty-stream) ;=> #t     (null? stream-empty) ;=> #f
    ;; (stream-pair? '(1 2))    ;=> #f     (stream? '(1 2))     ;=> #t
    (check-true (stream-pair? (cons-stream 1 2)))
    (check-false (stream-pair? '(1 2)))
    (check-true (empty-stream? the-empty-stream))
    (check-equal? (stream-head (rkt:stream 0 1 2 3) 2) '(0 1)))
   (test-case
    "stream:for-each"
    (define x 0)
    (define (X . _) (set! x (+ x 1)))
    (check-equal? (stream:for-each X (rkt:stream 1 2 3) 0) '...)
    (check-equal? (stream:for-each X (rkt:stream) 0) 'done)
    (check-equal? x 0)
    (check-equal? (stream:for-each X (rkt:stream 1 2 3) 1) '...)
    (check-equal? x 1)
    (check-equal? (stream:for-each X (rkt:stream 1 2 3) 10) 'done)
    (check-equal? x 4)
    (check-equal? (stream:for-each X (rkt:stream 1 2 3)) 'done)
    (check-equal? x 7))
   (test-case
    "print-stream"
    (check-equal? (out->string (check-equal? (print-stream (rkt:stream)) 'done))
                  "")
    (check-equal? (out->string (check-equal? (print-stream (rkt:stream 1 2)) 'done))
                  "1\n2\n")
    (check-equal? (out->string (check-equal? (print-stream (rkt:stream 1 2) 1) '...))
                  "1\n"))
   (test-case
    "combiner-padded-streams"
    (check-equal? (stream-head ((combiner-padded-streams list #f) (rkt:stream 1) (rkt:stream 1 2 3))  3)
                  '((1 1) (#f 2) (#f 3)))
    (check-equal? (stream-head ((combiner-padded-streams add1 3) (rkt:stream 1 2 3 4 5 6))  3)
                  '(2 3 4)))
   (test-case
    "infinite-streams"
    (let ([N (+ 10 (random 100))])
      (check-equal? (stream-head (stream-of-iterates add1 0) N)
                    (build-list N (λ (i) i))))
    (let ([N (+ 10 (random 100))][I (random 1000)])
      (check-equal? (stream-head (infinite-stream-of I) N)
                    (build-list N (λ (_) I)))))
   (test-case
    "map-stream"
    (define s:eta (stream-of-iterates add1 1))
    (check-equal? (stream->list (map-stream list (rkt:stream))) '())
    (check-equal? (stream->list (map-stream list (rkt:stream 1 2 3))) '((1) (2) (3)))
    (check-equal? (stream-head (map-stream list (stream-of-iterates add1 1)) 3) '((1) (2) (3)))
    (check-equal? (stream-head (map-streams * s:eta s:eta) 4) '(1 4 9 16))
    (check-equal? (stream->list (map-streams * the-empty-stream '())) '())
    ;; TODO: this is different than MIT, ok?
    (check-exn #px"map-streams: unequal length streams"
               (λ () (stream->list (map-streams + (rkt:stream 1) (rkt:stream 1 2))))))
   (test-case
    "stream-evaluate"
    (check-equal? (stream-head (stream-evaluate (stream-of-iterates (λ (f) (λ (x) (f (f x)))) add1) 0) 4)
                  '(1 2 4 8))
    (check-equal? (stream-head (stream-apply (stream-of-iterates (λ (f) (λ (x) (f (f x)))) add1) '(0)) 4)
                  '(1 2 4 8))
    (check-equal? (stream-head (stream-apply (stream-of-iterates (λ (f) (λ (x y) (* (f (f x y) y) y)))
                                                                 (λ (x y) (add1 x)))
                                             '(0 3)) 4)
                  '(1 6 72 6048)))
   (test-case
    "merge-streams"
    (check-equal? (stream-head (merge-streams (stream-of-iterates add1 1) (stream-of-iterates sub1 0)) 6)
                  '(1 0 2 -1 3 -2)))
   (test-case
    "shorten-stream"
    (check-equal? (stream->list (shorten-stream 3 (stream-of-iterates add1 1))) '(1 2 3))
    (check-equal? (stream->list (shorten-stream 3 (rkt:stream 1 2))) '(1 2)))
   (test-case
    "stream:+/-/*//"
    (check-equal? (stream->list (stream:+ (rkt:stream 1 4 9) (rkt:stream 2 3 1/2))) '(3 7 19/2))
    (check-equal? (stream->list (stream:- (rkt:stream 1 4 9) (rkt:stream 2 3 1/2))) '(-1 1 17/2))
    (check-equal? (stream->list (stream:* (rkt:stream 1 4 9) (rkt:stream 2 3 1/2))) '(2 12 9/2))
    (check-equal? (stream->list (stream:/ (rkt:stream 1 4 9) (rkt:stream 2 3 1/2))) '(1/2 4/3 18)))
   (test-case
    "standard streams"
    (let ([N (+ 10 (random 100))])
      (check-equal? (stream-head zero-stream N)
                    (build-list N (λ (_) 0))))
    (let ([N (+ 10 (random 100))])
      (check-equal? (stream-head one-stream N)
                    (build-list N (λ (_) 1))))
    (let ([N (+ 10 (random 100))][I (random 1000)])
      (check-equal? (stream-head (integers-starting-from I) N)
                    (build-list N (λ (i) (+ i I)))))
    (let ([N (+ 10 (random 100))])
      (check-equal? (stream-head natural-number-stream N)
                    (build-list N (λ (i) (+ i 1)))))
    (check-equal? (stream-head factorial-stream 5) '(1 1 2 6 24))
    (let ([N (+ 10 (random 100))])
      (check-equal? (stream-head (stream-of-powers 2 3) N)
                    (build-list N (λ (i) (* 3 (expt 2 i))))))
    (check-equal? (stream-head prime-numbers-stream 10)
                  '(2 3 5 7 11 13 17 19 23 29)))
   (test-case
    "stream-accumulate"
    (check-equal? (stream-head (stream-accumulate natural-number-stream) 3)
                  '(1 3 6))
    (check-equal? (stream-head (stream-accumulate natural-number-stream 3) 3)
                  '(4 6 9))
    ;; TODO : should this work with finite streams?
    (skip (check-equal? (stream->list (stream-accumulate (rkt:stream 1 2 3)))
                        '(1 3 6))))
   (test-case
    "stream:inflate"
    (check-equal? (stream-head (stream:inflate natural-number-stream 0) 10)
                  '(1 2 3 4 5 6 7 8 9 10))
    (check-equal? (stream-head (stream:inflate natural-number-stream 1) 10)
                  '(1 0 2 0 3 0 4 0 5 0))
    (check-equal? (stream-head (stream:inflate natural-number-stream 2) 10)
                  '(1 0 0 2 0 0 3 0 0 4))
    ;; TODO : should this work with finite streams?
    (skip (check-equal? (stream->list (stream:inflate (rkt:stream 1 2 3) 1))
                        '(1 0 2 0 3)))
    )
   (test-case
    "stream-list-append"
    (check-equal? (stream-head (stream:list-append '() natural-number-stream) 10)
                  '(1 2 3 4 5 6 7 8 9 10))
    (check-equal? (stream-head (stream:list-append '(a b c) natural-number-stream) 10)
                  '(a b c 1 2 3 4 5 6 7)))
   
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))