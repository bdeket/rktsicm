#lang racket/base

(require (for-syntax racket/base)
         ;; ../main.rkt shadows raise, in modules this is not a problem, but if you would use
         ;; check-simplified in the repl, I wan't to at least avoid one possible error...
         (rename-in racket/base [raise dont-shadow-my-raise])
         rackunit
         (only-in "../display/print.rkt" simplify)
         "../display/suppress-args.rkt"
         "helper.rkt"
         (submod "helper.rkt" runner))

(provide check-unique-match? check-simplified?
         accumulate out->string skip
         clear-arguments suppress-arguments rename-part)

;***************************************************************************************************
(define-syntax (check-simplified? stx)
  (syntax-case stx ()
    [(_ term pattern #:timeout t)
     (with-syntax ([loc (list (syntax-source stx)
                              (syntax-line stx)
                              (syntax-column stx)
                              (syntax-position stx)
                              (syntax-span stx))])
       #`(let* ([cT (make-channel)]
                [cP (make-channel)]
                [T (thread (λ () (channel-put cT (with-handlers ([exn? (λ (e) e)])
                                                   (simplify (rename-expression (arg-suppressor+ (simplify term))))))))]
                [P (thread (λ () (channel-put cP (with-handlers ([exn? (λ (e) e)])
                                                   (simplify (rename-expression (arg-suppressor+ (simplify pattern))))))))]
                [time t])
           (define vT (sync/timeout time cT))(kill-thread T)
           (when (exn? vT) (dont-shadow-my-raise vT))
           (define vP (and vT (sync/timeout time cP)))(kill-thread P)
           (when (exn? vP) (dont-shadow-my-raise vP))
           (cond
             [(eq? #f vT)
              (with-check-info* (list (make-check-name '|check-simplified?|)
                                      (make-check-params '(simplify term))
                                      (make-check-message (format "timeout = ~as => simplification/calculation too difficult?" t))
                                      (make-check-location 'loc)
                                      (check-info 'timeout #t))
                                fail)]
             [(eq? #f vP)
              (with-check-info* (list (make-check-name '|check-simplified?|)
                                      (make-check-params '(simplify pattern))
                                      (make-check-message (format "timeout = ~as => simplification/calculation too difficult?" t))
                                      (make-check-location 'loc)
                                      (check-info 'timeout #t))
                                fail)]
             [else
              (with-check-info*
               (list (make-check-name '|check-simplified?|)
                     (make-check-actual vT)
                     (make-check-expected vP)
                     (make-check-location 'loc))
               (λ () (check-equal? vT vP)))])))]
    [(_ term pattern)
     (syntax/loc stx (check-simplified? term pattern #:timeout (timeout)))]))

(module+ test
  (require "../main.rkt") ;; needed to use simplified
  (check-simplified? '(+ (* (sin x) (cos y)) (+ 4 5))
                     '(+ 9 (* (cos y) (sin x))))
  (check-exn exn:fail?
             (λ ()
               (check-simplified? '(+ 9 (* (sin x) (cos y)))
                                  '(+ 9 (* (sin y) (cos z))))))
  (check-exn exn:fail?
             (λ () (check-simplified? (let ([a 1])(sleep 100)a) 1 #:timeout .1))))
(module+ test
  (check-equal? (my-test-runner (test-suite "test-the-test"
                                            (check-true #t) ;; ok
                                            (check-true #f) ;; fail
                                            (check-true (/ 0)) ;; err
                                            (check-simplified? (let ([a 1]) (sleep 100) a) 'a #:timeout .1)
                                            (check-simplified? (let ([a 1]) (sleep 100) a) 1 #:timeout .1)
                                            (skip (check-true #t))
                                            (skip (check-true #f))
                                            ))
                #(1 1 1 2 2)))
