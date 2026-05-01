#lang racket/base

(require rackunit
         "../kernel-intr.rkt"
         "../simplify.rkt" ;; loading simplify => triggers error
         "helper+scm.rkt")

;; reentrant error
;; * problem with check-simplified? threads
;; * NOT TRUE: [ only in module (same test in interactions does not error) ]
;;     it can be triggered in interactions if it was not previously triggered in the definitions
;; * the error is in the first thread started (if T is started first that one errors,
;;      if P is started first, that one errors even if T is synced first)
;; * looking at the implementation (racket/collects/racket/private/stream-cons.rkt) this makes me
;;   think that the first thread starts calculating a difficult value of a stream
;;   then the second thread comes in trying to get the next value before the previous one is finished
;;   and the placeholder is this REENTRANT ERROR. (stream-force -> line 93)

;; short version that still consistently errors with check-simplified?
; (up '(/ (+ (* a d^) (* a^ d) (* b c^) (* b^ c)) (+ a b c d)))

;; original version
; (up '(/ (+ (* 2 a d^) (* -2 a^ d) (* -2 b c^) (* 2 b^ c)) (+ (expt a 2) (expt b 2) (expt c 2) (expt d 2))))

;; The fix (for this particular simplify): force the first 20 primes - others might need more
;; added in helper+scm
#; (void (stream-ref prime-numbers-stream 20))

(provide the-tests)

(define the-tests
  (test-suite
   "stream-reantrant error"
   (check-not-exn
    (λ ()
      (for ([i (in-range 10)])
        (check-simplified? (up '(/ (+ (* a d^) (* a^ d) (* b c^) (* b^ c)) (+ a b c d)))
                           (up '(/ (+ (* a d^) (* a^ d) (* b c^) (* b^ c)) (+ a b c d)))))))

   #; ;; checking twice is not useful, if the above test ran this test will be succesful (same number of primes needed)
   (check-false (ormap exn:fail?
                       (map thread-wait
                            (build-list 10 ;; with short version 2 is sometimes not enough
                                        (λ (_)
                                          (thread #:keep 'results
                                                  (λ ()
                                                    (with-handlers ([exn:fail? (λ (e) e)])
                                                      (g:simplify '(up (/ (+ (* a d^) (* a^ d) (* b c^) (* b^ c)) (+ a b c d))))))))))))


   (let ()
     ;; the actual problem
     (local-require racket/stream)
     (define K (stream-cons 1
                            (stream-cons 1
                                         (for/stream ([a (in-stream K)]
                                                      [b (in-stream (stream-rest K))])
                                           (let () (sleep 1)(+ a b))))))
     (check-true (ormap exn:fail?
                        (map thread-wait
                             (build-list 3
                                         (λ (_)
                                           (thread #:keep 'results
                                                   (λ () (with-handlers ([exn:fail? (λ (e) e)])
                                                           (stream-ref K 10))))))))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))