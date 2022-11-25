#lang s-exp "../../kernel.rkt"

(require rackunit
         "../../rkt/glue.rkt"
         (only-in "../../rkt/todo.rkt" pp)
         "../../general/assert.rkt"
         "../../simplify/sparse-interpolate.rkt"
         "../../simplify/sparse.rkt"
         "../../numerics/linear/lu.rkt"
         "../helper.rkt")

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
    (define (check m)
      (let ((xs (generate-list m interpolate-random))
            (fs (generate-list m interpolate-random)))
        (let ((t0 (current-milliseconds)))
          (univariate-interpolate-values xs fs
                                         (lambda (new-result)
                                           (let ((t1 (current-milliseconds)))
                                             (old-univariate-interpolate-values xs fs
                                                                                (lambda (old-result)
                                                                                  (let ((t2 (current-milliseconds)) (e (equal? old-result new-result)))
                                                                                    (assert (<= (- t1 t0) (- t2 t1)))
                                                                                    (assert e)))
                                                                                (lambda ()
                                                                                  (pp (list 'old-failed-new-won xs fs new-result))))))
                                         (lambda ()
                                           (let ((t1 (current-milliseconds)))
                                             (old-univariate-interpolate-values xs fs
                                                                                (lambda (old-result)
                                                                                  (pp (list 'new-failed-old-won xs fs old-result)))
                                                                                (lambda ()
                                                                                  (let ((t2 (current-milliseconds)))
                                                                                    (assert (<= (- t1 t0) (- t2 t1)))
                                                                                    (void))
                                                                                  'both-failed))))))))
    (check-equal? (let lp ((i 10))
                    (if (fix:= i 0)
                        'done
                        (begin (check (+ 15 (random 50)))
                               (lp (fix:- i 1)))))
                  'done))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))