#lang racket/base

(require (for-syntax racket/base)
         ;; ../main.rkt shadows raise, in modules this is not a problem, but if you would use
         ;; check-simplified in the repl, I wan't to at least avoid one possible error...
         (rename-in racket/base [raise dont-shadow-my-raise])
         rackunit
         racket/match
         racket/port
         racket/list
         syntax/srcloc
         racket/syntax
         (only-in "../display/print.rkt" simplify)
         "../display/suppress-args.rkt")

(provide check-unique-match? check-simplified?
         accumulate out->string skip
         clear-arguments suppress-arguments rename-part)

;***************************************************************************************************
(define-syntax (check-unique-match? stx)
  (syntax-case stx ()
    [(_ term (ids ...) pattern)
     #'(check-unique-match? term (ids ...) pattern #:when #t)]
    [(_ term (ids ...) pattern #:when guard)
     (with-syntax ([loc (list (syntax-source stx)
                              (syntax-line stx)
                              (syntax-column stx)
                              (syntax-position stx)
                              (syntax-span stx))])
       #`(with-check-info*
          (list (make-check-name '|check-unique-match?|)
                (make-check-actual term)
                (make-check-expected 'pattern)
                (make-check-location 'loc))
          (λ ()
            (check-true
             (match term
               [pattern
                 #:when guard
                (if (= (length '(ids ...))
                       (length (remove-duplicates (list ids ...))))
                    #t
                    #f)]
               [else #f])))))]))

(module+ test
  (let ([a (gensym)]
        [b (gensym)])
    (check-unique-match? `(+ (* 4 ,a)
                             (* ,a ,b))
                         (x y)
                         `(+ (* 4 ,x)
                             (* ,x ,y))))
  (let ([a (gensym)]
        [b (gensym)])
    (check-unique-match? `((+ 4 ,a)
                           (* ,a ,b))
                         (x y)
                         (list-no-order `(* ,x ,y)
                                        `(+ 4 ,z))
                         #:when (eq? x z)))
  
  (check-exn exn:fail?
             (λ ()
               (let ([a (gensym)]
                     [b (gensym)])
                 (check-unique-match? `(+ (* 4 ,a)
                                          (* ,a ,b))
                                      (x y)
                                      `(+ (* 4 ,x)
                                          (* ,y ,x))))))

  (check-exn exn:fail?
             (λ ()
               (let ([a (gensym)]
                     [b (gensym)])
                 (check-unique-match? `(+ (* 4 ,a)
                                          (* ,a ,b))
                                      (x y z)
                                      `(+ (* 4 ,x)
                                          (* ,y ,z)))))))

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
;***************************************************************************************************
(define-syntax-rule (accumulate acc expr)
  (let* ([val '()]
         [acc (λ (v) (set! val (append val (list v))))])
    expr
    val))
;***************************************************************************************************
(define-syntax-rule (out->string body ...)
  (call-with-output-string (λ (out) (parameterize ([current-output-port out]) body ...))))

;***************************************************************************************************

;***************************************************************************************************
(module runner racket/base
  (require rackunit
           rackunit/private/format
           (for-syntax racket/base))
  (provide my-test-runner timeout skip)
  (define timeout (make-parameter #f))
  (define skipper (make-parameter #f))
  (define-syntax (skip stx)
  (syntax-case stx ()
    [(_ body ...)
     (with-syntax ([loc (list (syntax-source stx)
                              (syntax-line stx)
                              (syntax-column stx)
                              (syntax-position stx)
                              (syntax-span stx))])
       #`(with-check-info*
          (list (make-check-name 'skip)
                (make-check-location 'loc)
                (check-info 'skip #t))
          (λ () (check-true (begin (skipper #t) #t)))))]))

  (define my-test-runner
    (let ([seed (make-vector 5 0)])
      (define (make i) (λ (v) (vector-set! v i (+ (vector-ref v i) 1)) v))
      (define succes (make 0))
      (define fail (make 1))
      (define error (make 2))
      (define timeout (make 3))
      (define skip (make 4))
      (λ (test)
        (fold-test-results (λ (rslt seed)
                             ((cond
                                [(test-success? rslt)
                                 (if (skipper) (begin (skipper #f) skip) succes)]
                                [(test-failure? rslt)
                                 (define exn (test-failure-result rslt))
                                 (display-test-failure/error exn)
                                 (cond
                                   [(exn:test:check? exn)
                                    (define stack (exn:test:check-stack exn))
                                    (let lp ([ci stack])
                                      (cond
                                        [(null? ci) fail]
                                        [else
                                         (or (case (check-info-name (car ci))
                                               [(timeout) (and (check-info-value (car ci)) timeout)]
                                               [else #f])
                                             (lp (cdr ci)))]))]
                                   [else fail])]
                                [(test-error? rslt)
                                 (display-test-failure/error (test-error-result rslt))
                                 error])
                              seed))
                           seed
                           test)))))
(require (submod "." runner))

(module+ test
  (require (submod ".." runner))
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