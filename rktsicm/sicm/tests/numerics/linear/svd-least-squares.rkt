#lang racket/base

(require rackunit
         "../../../numerics/linear/svd-least-squares.rkt"
         (only-in "../../../rkt/glue.rkt" iota list-head)
         "../../../kernel-intr.rkt"
         (only-in "../../../numerics/statistics/gauss.rkt" gaussian-random)
         )
(kernel:assign-operations)

(provide the-tests)
(define the-tests
  (test-suite
   "numerics/linear/svd-least-squares"
   (test-case
    ""
    (define ((f A B) x)
      (+ (* A (cos x)) (* B (* 2 (sin x)))))

    (define (make-random-data A B sigma xs)
      (list->vector
       (map (lambda (x)
              (+ ((f A B) x) (* sigma (gaussian-random))))
            xs)))

    (define (make-design xs)
      (apply matrix-by-rows
             (map (lambda (x) (list (cos x) (* 2 (sin x)))) xs)))

    (define (xs n)
      (map (lambda (i) (* 2pi (/ i n))) (iota n)))

    (define (errors sigma var n)
      (v:generate n (lambda (i) (+ sigma (* var (gaussian-random))))))

    (define test-xs (xs 100))
    (define test-design (make-design test-xs))
    (define σ .05)
    (define test-b (make-random-data 1 10 σ test-xs))

    (check-within (list-head (svd-least-squares test-design test-b 2) 2)
                  (list (up 1. 10.)
                        (up 0.37606030930863943 0.26591479484724945))
                  (* 3 σ))
    (check-within (list-head (svd-least-squares test-design test-b 2 (errors .1 .01 100)) 2)
                  (list (up 1. 10.)
                        (up 0.11816866685497388 0.08298722096330267))
                  (* 3 σ)))
#|

(svd-least-squares test-design test-b 2)
#|
((up 1.0181940511405976 9.992722481700229)
 (up .37606030930863943 .26591479484724945)
 1.1211970577354886)
|#

(svd-least-squares test-design test-b 2 (errors .1 .01 100))
#|
((up 1.0156989475194924 9.987917234874896)
 (up .11796687913681762 .0840107105487443)
 1.1261264142499618)
|#

|#
      ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))