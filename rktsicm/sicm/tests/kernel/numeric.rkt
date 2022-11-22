#lang racket/base

(require rackunit
         "../../main.rkt"
         "../../rkt/int.rkt"
         "../../rkt/fixnum.rkt"
         "../helper.rkt"
         )

(define kernel-tests
  (test-suite
   "kernel/numeric"
   (test-case
    "ORIG:stirling-first-kind"
    (check-equal? (stirling-first-kind 1 1) 1)
    (check-equal? (stirling-first-kind 2 1) 1)
    (check-equal? (stirling-first-kind 2 2) 1)
    (check-equal? (stirling-first-kind 3 1) 2)
    (check-equal? (stirling-first-kind 3 2) 3)
    (check-equal? (stirling-first-kind 5 2) 50)
    (check-equal? (stirling-first-kind 7 3) 1624)
    )

   (test-case
    "ORIG:stirling-second-kind"
    (check-equal? (stirling-second-kind 5 3) 25)
    )

   (test-case
    "ORIG:sigma"
    (check-equal? (sigma (lambda (x) (/ 1.0 x)) 1 10000000)
                  16.695311365857272))
   (test-case
    "harmonic - slow"
    (define (Hn n)
      (/ (stirling-first-kind (+ n 1) 2)
         (factorial n)))
    (check-simplified? (exact->inexact (Hn 300))
                       6.282663880299504
                       #:timeout 3))
   (test-case
    "harmonic"
    (check-equal? (let lp ((i 1) (sum 0.0))
                    (if (> i 300)
                        sum
                        (lp (+ i 1) (+ sum (/ 1.0 i)))))
                  6.282663880299502)

    (check-equal? (let lp ((i 300) (sum 0.0))
                    (if (= i 0)
                        sum
                        (lp (- i 1) (+ sum (/ 1.0 i)))))
                  6.282663880299501)

    (check-equal? (sigma (lambda (x) (/ 1.0 x)) 1 300)
                  6.282663880299502))
   (test-case
    "geometric"
    (define write-line values)
    (define (geometric a r n)
      (define (sigma-KahanBabushkaNeumaier f low high)
        (let lp ((i low) (sum 0) (c 0))
          (if (fix:> i high)
              sum
              (let* ((y (- (f i) c)) (t (+ sum y)))
                (lp (fix:+ i 1) t (- (- t sum) y))))))
      (list (let lp> ((k 0) (sum 0.0))
              (if (> k n)
                  (write-line `(forward-order ,sum))
                  (lp> (+ k 1) (+ sum (* a (expt r k))))))
            (write-line
             `(g:sigma
               ,(g:sigma				;generic sigma is naive
                 (lambda (k)
                   (exact->inexact (* a (expt r k))))
                 0 n)))
            (let lp< ((k n) (sum 0.0))
              (if (< k 0)
                  (write-line `(reverse-order ,sum))
                  (lp< (- k 1) (+ sum (* a (expt r k))))))
            (write-line
             `(kahan-method
               ,(sigma-KahanBabushkaNeumaier
                 (lambda (k)
                   (exact->inexact (* a (expt r k))))
                 0 n)))  
            (write-line
             `(explicit-formula
               ,(exact->inexact (/ (* a (- 1 (expt r (+ n 1))))
                                   (- 1 r)))))))

    (check-equal? (geometric 1 0.5001 200)
                  '((forward-order 2.0004000800160022)
                    (g:sigma 2.0004000800160022)
                    (reverse-order 2.000400080016003)
                    (kahan-method 2.000400080016003)
                    (explicit-formula 2.000400080016003))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests kernel-tests))