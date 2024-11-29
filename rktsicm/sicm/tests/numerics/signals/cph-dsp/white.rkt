#lang racket/base

(require rackunit
         "../../../../numerics/signals/cph-dsp/white.rkt"
         "../../../../numerics/signals/cph-dsp/flovec-scm.rkt"
         racket/flonum
         )

(provide the-tests)
(define the-tests
  (test-suite
   "numerics/signals/cph-dsp/white"
   (test-case
    "make-noise-vector"
    (let* ([state (make-pseudo-random-generator)]
           [sttv (pseudo-random-generator->vector state)])
      (check-equal? (flonum-vector->vector (make-noise-vector random state 4))
                    (parameterize ([current-pseudo-random-generator (vector->pseudo-random-generator sttv)])
                      (for/vector ([i (in-range 4)])
                        (random))))))
   (test-case
    "make-uniform-noise-vector"
    (test-case
     "make-noise-vector"
     (let* ([state (make-pseudo-random-generator)]
            [sttv (pseudo-random-generator->vector state)])
       (check-equal? (flonum-vector->vector (make-uniform-noise-vector state 4))
                     (parameterize ([current-pseudo-random-generator (vector->pseudo-random-generator sttv)])
                       (flonum-vector->vector (make-uniform-noise-vector (current-pseudo-random-generator) 4)))))))
   (test-case
    "make-gaussian-noise-vector:polar-method"
    ;; not sure what to test except that it creates a vector of correct length
    ;; and repeatable
    (let* ([state (make-pseudo-random-generator)]
           [sttv (pseudo-random-generator->vector state)])
      (define V (flonum-vector->vector (make-gaussian-noise-vector:polar-method state 6)))
      (check-equal? (vector-length V) 6)
      (check-equal? V
                    (parameterize ([current-pseudo-random-generator (vector->pseudo-random-generator sttv)])
                      (flonum-vector->vector (make-gaussian-noise-vector:polar-method (current-pseudo-random-generator) 6)))))
    (let* ([state (make-pseudo-random-generator)]
           [sttv (pseudo-random-generator->vector state)])
      (define V (flonum-vector->vector (make-gaussian-noise-vector:polar-method state 7)))
      (check-equal? (vector-length V) 8))
    (random-seed 1)
    (make-gaussian-noise-vector:polar-method (current-pseudo-random-generator) 7))
   (test-case
    "marsaglia-maclaren-method"
    (random-seed 1)
    (build-list 10
                (λ (_)
                  ((marsaglia-maclaren-method  one-sided-unit-gaussian-pdf
                                               one-sided-unit-gaussian-pdf-limits
                                               2 2)
                   (current-pseudo-random-generator))))
    (build-list 10
                (λ (_)
                  ((marsaglia-maclaren-method  one-sided-unit-gaussian-pdf
                                               one-sided-unit-gaussian-pdf-limits
                                               256 128)
                   (current-pseudo-random-generator))))
    (build-list 100
                (λ (_)
                  ((marsaglia-maclaren-method  one-sided-unit-gaussian-pdf
                                               one-sided-unit-gaussian-pdf-limits
                                               1 1)
                   (current-pseudo-random-generator)))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))