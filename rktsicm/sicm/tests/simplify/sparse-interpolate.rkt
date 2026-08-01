#lang s-exp "../../generic.rkt"

(require rackunit
         "../../rkt/glue.rkt"
         (only-in "../../display/pp.rkt" pp)
         "../../general/assert.rkt"
         "../../simplify/sparse-interpolate.rkt"
         "../../simplify/sparse.rkt"
         "../../numerics/linear/lu.rkt"
         "../helper.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "simplify/sparse"
   (test-case
    "interpolate-random"
    (set!-interpolate-size 1)
    (check-equal? (interpolate-random 'any) 1)
    (set!-interpolate-size 5)
    (check-true (<= 1 (interpolate-random 'and) 5))
    (set!-interpolate-size 10000)
    (check-true (<= 1 (interpolate-random 'all) 10000)))
   (test-case
    "univariate-interpolate"
    (check-equal? (univariate-interpolate-values '(2) '(5) vector list)
                  (vector (list (sparse-term '(0) 5))))
    (check-equal? (univariate-interpolate-values '(0 1) '(3 2) list vector) ;; 3-x
                  (list (list (sparse-term '(1) -1) (sparse-term '(0) 3))))
    (check-equal? (univariate-interpolate-values '(516 516) '(1804 3283)
                                                 error (λ _ 'fail))
                  'fail)

    (check-equal? (univariate-interpolate (λ (x) 3) 0)
                  (list (sparse-term '(0) 3)))
    (check-equal? (univariate-interpolate (λ (x) (- 3 x)) 1)
                  (list (sparse-term '(1) -1) (sparse-term '(0) 3)))
    (random-seed 19168436) ;; forces the first interpolation to fail (double xs generated)
    (check-equal? (univariate-interpolate (λ (x) (- 3 x)) 1)
                  (list (sparse-term '(1) -1) (sparse-term '(0) 3))))
   (test-case
    "expand-poly"
    (check-equal? (expand-poly '() '()) '())
    (check-equal? (expand-poly '( ((5) . 3) ) '( () )) '())
    (check-equal? (expand-poly '( ((5) . 3) ) '( (((1) . 2)) )) '( ((5 1) . 2) ))
    (check-equal? (expand-poly '( ((5) . 3) ) '( (((1) . 2) ((3) . 4)) )) '( ((5 3) . 4) ((5 1) . 2))))
   (test-case
    "interpolate-skeleton"
    (random-seed 1)
    (check-equal? (interpolate-skeleton (λ (x) ((λ (y) (+ (* x x) (* y y))) 3))
                                        '(((2) . 4) ((0) . 5)))
                  '(((2) . 1) ((0) . 9)))
    (random-seed 14761) ;; forcing try-again
    (check-equal? (interpolate-skeleton (λ (x) ((λ (y) (+ (* x x) (* y y))) 3))
                                        '(((2) . 4) ((0) . 5)))
                  '(((2) . 1) ((0) . 9)))
    (set!-interpolate-skeleton-using-vandermonde? #f)
    (random-seed 1)
    (check-equal? (interpolate-skeleton (λ (x) ((λ (y) (+ (* x x) (* y y))) 3))
                                        '(((2) . 4) ((0) . 5)))
                  '(((2) . 1) ((0) . 9)))
    (random-seed 7712) ;; forcing try-again
    (check-equal? (interpolate-skeleton (λ (x) ((λ (y) (+ (* x x) (* y y))) 3))
                                        '(((2) . 4) ((0) . 5)))
                  '(((2) . 1) ((0) . 9)))
    (set!-interpolate-skeleton-using-vandermonde? #t))
   (test-case
    "sparse-interpolate"
    (random-seed 1)
    (check-equal? (sparse-interpolate (λ (x y) (+ (* x x) (* y y))) 2 2)
                  '(((2 0) . 1) ((0 2) . 1)))
    (random-seed 3764)
    (check-equal? (sparse-interpolate (λ (x y) (+ (* x x) (* y y))) 2 2)
                  '(((2 0) . 1) ((0 2) . 1))))
   ;**************************************************************************************************
   (check-equal? (expand-poly '(((5) . 3) ((2) . 1) ((1) . 1) ((0) . 4))
                              '( (((1) . 1) ((0) . 3))
                                 (((1) . 1))
                                 (((3) . 2) ((0) . 4))
                                 (((1) . 2) ((0) . 5)) ))
                 '(((5 1) . 1) ((5 0) . 3) ((1 3) . 2) ((2 1) . 1) ((1 0) . 4) ((0 1) . 2) ((0 0) . 5)))
   (check-equal? (sparse-interpolate
                  (lambda (x y z) (+ (* 3 (square x) (cube y)) (* x y z) (* 4 z) 1))
                  3
                  4)
                 '(((2 3 0) . 3) ((1 1 1) . 1) ((0 0 1) . 4) ((0 0 0) . 1)))
   (check-equal? (interpolate-skeleton
                  (lambda (x) (+ (* 3 (expt x 5)) (expt x 2) x 4))
                  '(((5) . 1) ((2) . 1) ((1) . 1) ((0) . 1)))
                 '(((5) . 3) ((2) . 1) ((1) . 1) ((0) . 4)))

   (check-equal? (univariate-interpolate
                  (lambda (x) (+ (* 3 (expt x 5)) (expt x 2) x 4))
                  6)
                 '(((5) . 3) ((2) . 1) ((1) . 1) ((0) . 4)))
   (test-case
    "compare old-new"
    (define (old-univariate-interpolate-values xs fs succeed fail)
      (let ((n (length xs)))
        (assert (fix:= n (length fs)))
        (let* ((exponents (iota n))
               (matrix
                (matrix-by-row-list
                 (map (lambda (x)
                        (map (lambda (e) (expt x e))
                             exponents))
                      xs))))
          (lu-solve matrix
                    (list->vector fs)
                    (lambda (coefficients)
                      (succeed (reverse
                                (filter (lambda (term)
                                          (not (zero? (sparse-coefficient term))))
                                        (map (lambda (exponent coefficient)
                                               (sparse-term (list exponent)
                                                            coefficient))
                                             exponents
                                             (vector->list coefficients))))))
                    (lambda (ignore) (fail))))))
    ;;; Check that the new algorithm is equivalent to the old one, and faster
    (define timings (make-hash))
    (define (check m)
      (let ((xs (generate-list m interpolate-random))
            (fs (generate-list m interpolate-random)))
        (let ((t0 (current-milliseconds)))
          (univariate-interpolate-values xs fs
                                         ;; succeed
                                         (lambda (new-result)
                                           (let ((t1 (current-milliseconds)))
                                             (old-univariate-interpolate-values xs fs
                                                                                (lambda (old-result)
                                                                                  (let ((t2 (current-milliseconds)) (e (equal? old-result new-result)))
                                                                                    (assert e)
                                                                                    (hash-update! timings '++ (λ (x) (list (+ (car x) 1)
                                                                                                                           (+ (cadr x) (/ (- t1 t0) (- t2 t1) 1.))
                                                                                                                           (max (caddr x) (/ (- t1 t0) (- t2 t1) 1.))))
                                                                                                  (list 0 0 0))
                                                                                    #;(assert (<= (- t1 t0) (- t2 t1))
                                                                                            (format "1 (~a ~a) -- " (/ (- t1 t0) (- t2 t1) 1.) m))))
                                                                                (lambda ()
                                                                                  (hash-update! timings '+- add1 0)
                                                                                  'old-failed-new-won))))
                                         ;; fail
                                         (lambda ()
                                           (let ((t1 (current-milliseconds)))
                                             (old-univariate-interpolate-values xs fs
                                                                                (lambda (old-result)
                                                                                  (hash-update! timings '-+ add1 0)
                                                                                  (pp (list 'new-failed-old-won xs fs old-result)))
                                                                                (lambda ()
                                                                                  (let ((t2 (current-milliseconds)))
                                                                                    (hash-update! timings '-- (λ (x) (list (+ (car x) 1)
                                                                                                                           (+ (cadr x) (/ (- t1 t0) (- t2 t1) 1.))
                                                                                                                           (max (caddr x) (/ (- t1 t0) (- t2 t1) 1.))))
                                                                                                  (list 0 0 0))
                                                                                    #;(assert (<= (- t1 t0) (- t2 t1))
                                                                                            (format "2 (~a ~a) -- " (/ (- t1 t0) (- t2 t1) 1.) m))
                                                                                    (void))
                                                                                  'both-failed))))))))
    (check-equal? (let lp ((i 10))
                    (if (fix:= i 0)
                        'done
                        (begin (check (+ 15 (random 50)))
                               (lp (fix:- i 1)))))
                  'done)
    (println timings)
    ;; Old never better
    (check-false (hash-ref timings '-+ #f))
    ;; when ok (~93% of cases): new never slower
    (check-true (let ([X (hash-ref timings '++ '(0 0 0))])
                  (or (<= (caddr X) 1.)
                      ;; almost never
                      (and (<= (/ (cadr X) (car X)) 0.9) (<= (caddr X) 3.5)))))
    ;; when fail (~7% of cases): new generally not slower
    (check-true (let ([X (hash-ref timings '++ '(0 0 0))])
                  (or ;; almost never
                      (<= (caddr X) 1.)
                      ;; usually ~ 3
                      (and (<= (caddr X) 5.0)
                           (case (car X)
                             [(1) #t]
                             [(2) (<= (/ (cadr X) (car X)) 3.)]
                             [(3 4) (<= (/ (cadr X) (car X)) 2.5)]
                             [(5 6 7 8 9 10) (<= (/ (cadr X) (car X)) 2.)]
                             [else (<= (/ (cadr X) (car X)) 1.75)])))))
    )
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))