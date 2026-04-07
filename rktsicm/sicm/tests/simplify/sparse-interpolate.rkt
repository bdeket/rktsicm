#lang s-exp "../../generic.rkt"

(require rackunit
         "../../rkt/glue.rkt"
         (only-in "../../rkt/todo.rkt" pp)
         "../../general/assert.rkt"
         "../../simplify/sparse-interpolate.rkt"
         "../../simplify/sparse.rkt"
         "../../numerics/linear/lu.rkt"
         "../helper.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "simplify/sparse"
   (check-equal? (sparse-interpolate
                  (lambda (x y z) (+ (* 3 (square x) (cube y)) (* x y z) (* 4 z) 1))
                  3
                  4)
                 '(((2 3 0) . 3) ((1 1 1) . 1) ((0 0 1) . 4) ((0 0 0) . 1)))
   (check-equal? (interpolate-skeleton
                  (lambda (x) (+ (* 3 (expt x 5)) (expt x 2) x 4))
                  '(((5) . 1) ((2) . 1) ((1) . 1) ((0) . 1)))
                 '(((5) . 3) ((2) . 1) ((1) . 1) ((0) . 4)))
   (check-equal? (expand-poly '(((5) . 3) ((2) . 1) ((1) . 1) ((0) . 4))
                              '( (((1) . 1) ((0) . 3))
                                 (((1) . 1))
                                 (((3) . 2) ((0) . 4))
                                 (((1) . 2) ((0) . 5)) ))
                 '(((5 1) . 1) ((5 0) . 3) ((1 3) . 2) ((2 1) . 1) ((1 0) . 4) ((0 1) . 2) ((0 0) . 5)))
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