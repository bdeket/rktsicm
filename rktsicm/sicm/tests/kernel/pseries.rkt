#lang racket/base

(require rackunit
         racket/stream
         "../../kernel-intr.rkt"
         "../helper.rkt"
         )

(define ((s->l n) S) (stream->list (stream-take (series->stream S) n)))
(provide the-tests)
(define the-tests
  (test-suite
   "kernel/pseries"
   ;; TODO
   ;; series could really use a zero stream to pre-emptively stop series that terminate
   (test-case
    "creation"
    (check-equal? (series:type series:one) series-type-tag)
    (check-true ((series:type-predicate series:one) series:one))
    (define S? (series:type-predicate series:one))
    (check-true ((series:type-predicate (make-series 1 (stream 1))) series:one))
    (check-false ((series:type-predicate series:one) #f))
    (check-equal? (series:arity series:one) 1)
    (check-equal? (series:arity (make-series 10 (stream 1))) 10)
    (check-equal? (series:arity (make-series 10 (stream 1))) 10)
    (check-true (S? (series:promote-arity (make-series 0 (stream 1)))))
    (check-equal? (series:arity (series:promote-arity (make-series 0 (stream 1)))) 1)
    (check-exn #px"Not a series" (λ () (series->stream 4)))
    (check-equal? (series:same-arity (list series:one series:zero (make-series 1 (stream 1)))) 1)
    (check-equal? (series:same-arity '()) (arity-at-least 0))
    (check-false (series:same-arity (list series:one (make-series 10 (stream 10)))))
    (define N (series:generate values 3))
    (check-equal? (series:arity N) 3)
    (check-equal? (series:arity (series:generate values)) 1)
    (check-true (S? N))
    (check-equal? (series:ref N 0) 0)
    (check-equal? (series:ref N 5) 5)
    (check-equal? (series:zero-like series:one) series:zero)
    (check-equal? (series:one-like series:zero) series:one))
   (test-case
    "stream<->series"
    (check-true (stream? (series->stream series:one)))
    (define S ((series-wrapper (λ (s0 s1) (for/stream ([e0 (in-stream s0)]
                                                       [e1 (in-stream s1)])
                                            (+ e0 e1))))
               series:one series:one))
    (check-equal? (series:arity S) 1)
    (check-equal? (series:ref S 0) 2)
    (check-equal? (series:ref S 1) 0))
   (test-case
    "for each"
    (define Nat (series:generate values))
    (check-equal? (accumulate acc (series:for-each acc Nat 5)) '(0 1 2 3 4))
    (check-equal? (accumulate acc (series:for-each acc ((series:elementwise +) Nat series:one) 5))
                  '(1 1 2 3 4)))
   (test-case
    "series..."
    (check-equal? ((s->l 5) (series (up 1) (up 5)))
                  (list (up 1) (up 5) (up 0) (up 0) (up 0)))
    (check-equal? (series:arity (series (up 1) (up 5))) 0)
    (check-equal? ((s->l 5) (series:value (power-series 1 5) '(2)))
                  '(1 10 0 0 0))
    (check-equal? (series:arity (power-series 1 5)) 1)
    (check-equal? ((s->l 5) (series:value series:identity '(8)))
                  '(0 8 0 0 0))
    (check-equal? ((s->l 5) (series:value (constant-series 3) '(8)))
                  '(3 0 0 0 0))
    (check-equal? (series:arity (constant-series 3 3)) 3))
   (test-case
    "+/-/*// constant"
    (define q1 (infinite-stream-of 1))
    (define s1 (make-series 1 q1))
    (check-equal? ((s->l 5) (coefficient+series 5 s1))  '(6 1 1 1 1))
    (check-equal? ((s->l 5) (series+coefficient s1 5))  '(6 1 1 1 1))
    (check-equal? ((s->l 5) (coefficient-series 5 s1))  '(4 -1 -1 -1 -1))
    (check-equal? ((s->l 5) (series-coefficient s1 5))  '(-4 1 1 1 1))
    (check-equal? ((s->l 5) (coefficient*series 5 s1))  '(5 5 5 5 5))
    (check-equal? ((s->l 5) (series*coefficient s1 5))  '(5 5 5 5 5))
    (check-equal? ((s->l 5) (coefficient/series 5 s1))  '(5 -5 0 0 0))
    (check-equal? ((s->l 5) (series/coefficient s1 5))  '(1/5 1/5 1/5 1/5 1/5))
    (check-equal? (stream->list (stream-take (stream:c*s 5 q1) 5)) '(5 5 5 5 5))
    (check-equal? (stream->list (stream-take (stream:s/c q1 5) 5)) '(1/5 1/5 1/5 1/5 1/5)))
   (test-case
    "mul / div"
    (define s1 (make-series 1 (infinite-stream-of 1)))
    (check-equal? ((s->l 5) (series:mul s1 s1))     '(1 2 3 4 5))
    (check-equal? ((s->l 5) (mul$series s1 s1 s1))  '(1 3 6 10 15))
    (check-equal? ((s->l 5) (series:invert s1))     '(1 -1 0 0 0))
    (check-equal? ((s->l 5) (series:div s1 s1))     '(1 0 0 0 0))
    (check-equal? ((s->l 5) (div$series s1 s1 s1))  '(1 -1 0 0 0)))
   (test-case
    "expt"
    (define s1 (make-series 1 (infinite-stream-of 1)))
    (check-equal? (series:expt s1 0) series:one)
    (check-equal? (series:expt s1 1) s1)
    (check-equal? ((s->l 5) (series:expt s1 6))
                  '(1 6 21 56 126))
    (check-within (series:sum (series:value (series:expt (power-series 1 1 1) 1.5) '(1)) 11)
                  (expt (+ 1 1 1) 1.5) 1e-2)
    (check-equal? (expression ((s->l 3) (series:expt (power-series 1 1) 'a)))
                  '(1 a (/ (* a (+ -1 a)) 2)))
    (check-equal? ((s->l 5) (series:expt s1 -1))
                  '(1 -1 0 0 0)))
   (test-case
    "deriv / integr"
    (define s1 (make-series 1 (infinite-stream-of 1)))
    (check-equal? ((s->l 5) (series:derivative s1 '()))
                  '(1 2 3 4 5))
    (check-equal? (expression ((series:ref (series:derivative (series (λ (t) (g:* 3 t t))) '()) 0) 't))
                  '(+ (* 3 t) (* 3 t)))
    (check-exn #px"Cannot yet take partial derivatives of a series" (λ () (series:derivative s1 '(0))))
    (check-exn #px"Cannot take derivative of non arity=1 " (λ () (series:derivative (make-series 5 (series->stream series:one)) '())))
    (check-equal? ((s->l 5) (*integrate-series s1 'a))
                  '(a 1 1/2 1/3 1/4)))
   (test-case
    "series:value ->function function->"
    (define S5 (make-series 5 (cons-stream (λ (a b c d e) e)
                                           (series->stream series:one))))
    (define s1 (make-series 1 (infinite-stream-of 1)))
    (define ss1 (make-series 0 (infinite-stream-of s1)))
    (check-equal? (series:ref (series:value (series (λ (q r) (* q r))) '(2 3)) 0) 6)
    (check-equal? (series:ref (series:value series:one '(3)) 0) 1)
    (check-exn #px"Wrong number of args to series" (λ () (series:ref (series:value series:one '(3 4)) 0)))
    (check-exn #px"Bad arity series"
               (λ () (series:value S5 '(1 2 3 4 5))))

    (check-equal? ((s->l 5) (series:value s1 '(1))) '(1 1 1 1 1))
    (check-equal? ((s->l 5) (series:value (make-series 0 (infinite-stream-of (λ (x) x))) '(1)))
                  '(1 1 1 1 1))
    (check-equal? ((s->l 5) (series:value ss1 '(1))) '(1 2 3 4 5))

    (check-equal? (series:ref (series:value (series:->function (series 1.5)) '(5)) 0) 1.5)
    (check-equal? (series:->function series:one) series:one)
    (check-exn #px"Wrong arity SERIES:->FUNCTION" (λ () (series:->function S5)))

    (check-equal? ((s->l 5) (series:function-> (λ (x) x))) '(0 1 0 0 0))
    (check-equal? ((s->l 5) (series:function-> (λ (x) x) 1)) '(1 1 0 0 0)))
   (test-case
    "inflate"
    (define s1 (make-series 1 (infinite-stream-of 1)))
    (check-equal? ((s->l 5) (series:inflate s1 3)) '(1 0 0 1 0)))
   (test-case
    "solve-linear"
    
    (check-equal? ((s->l 5) (g:solve-linear (power-series 5) (power-series 3)))
                  ((s->l 5) (power-series 3/5)))
    (check-equal? ((s->l 5) (g:solve-linear 5 (power-series 3)))
                  ((s->l 5) (power-series 3/5)))
    (check-equal? ((s->l 5) (g:solve-linear (power-series 5) 3))
                  ((s->l 5) (power-series 3/5)))
    (check-equal? ((s->l 5) (g:solve-linear-left (power-series 5) (power-series 3)))
                  ((s->l 5) (power-series 3/5)))
    (check-equal? ((s->l 5) (g:solve-linear-left 5 (power-series 3)))
                  ((s->l 5) (power-series 3/5)))
    (check-equal? ((s->l 5) (g:solve-linear-left (power-series 5) 3))
                  ((s->l 5) (power-series 3/5))))
   (test-case
    "special"
    (check-equal? ((s->l 5) (binomial-series 0)) '(1 0 0 0 0))
    (check-equal? ((s->l 5) (binomial-series 3)) '(1 3 3 1 0))
    (check-equal? ((s->l 7)  cos-series) '(1 0 -1/2    0  1/24    0   -1/720))
    (check-equal? ((s->l 7)  sin-series) '(0 1    0 -1/6    0   1/120    0))
    (check-equal? ((s->l 7)  exp-series) '(1 1  1/2  1/6  1/24  1/120  1/720))
    (check-equal? ((s->l 7) cosh-series) '(1 0  1/2    0  1/24    0    1/720))
    (check-equal? ((s->l 7) sinh-series) '(0 1    0  1/6    0   1/120    0))
    (check-equal? ((s->l 7)  tan-series) '(0 1    0  1/3    0   2/15     0))
    (check-equal? ((s->l 7) atan-series) '(0 1    0 -1/3    0   1/5      0)))
   ;******************************************************************
   ;in main
   (test-case
    "series:print"
    (check-equal? (out->string (series:print exp-series 5))
                  "1\n1\n1/2\n1/6\n1/24\n"))
   (test-case
    "series:sum"
    (check-equal? (series:sum (series:value exp-series '(1)) 3) 8/3)
    (check-exn #px"Cannot sum non arity=0 series" (λ () (series:sum exp-series 3))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))