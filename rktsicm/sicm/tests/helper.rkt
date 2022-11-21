#lang racket/base

(require (for-syntax racket/base)
         ;; ../main.rkt shadows raise, in modules this is not a problem, but if you would use
         ;; check-simplified in the repl, I wan't to at least avoid one possible error...
         (rename-in racket/base [raise dont-shadow-my-raise])
         rackunit
         racket/match
         racket/list
         syntax/srcloc
         racket/syntax
         (only-in "../kernel-gnrc.rkt" simplify)
         "../display/suppress-args.rkt")

(provide check-unique-match? check-simplified?
         accumulate
         clear-arguments suppress-arguments rename-part)

;***************************************************************************************************
(define-syntax (check-unique-match? stx)
  (syntax-case stx ()
    [(_ term (ids ...) pattern)
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
                                                   (simplify (rename-expression (arg-suppressor+ (simplify pattern))))))))])
           (define vT (sync/timeout t cT))(kill-thread T)
           (when (exn? vT) (dont-shadow-my-raise vT))
           (define vP (and vT (sync/timeout t cP)))(kill-thread P)
           (when (exn? vP) (dont-shadow-my-raise vP))
           (cond
             [(eq? #f vT)
              (with-check-info* (list (make-check-name '|check-simplified?|)
                                      (make-check-params '(simplify term))
                                      (make-check-message (format "timeout = ~as => simplification/calculation too difficult?" t))
                                      (make-check-location 'loc))
                                fail)]
             [(eq? #f vP)
              (with-check-info* (list (make-check-name '|check-simplified?|)
                                      (make-check-params '(simplify pattern))
                                      (make-check-message (format "timeout = ~as => simplification/calculation too difficult?" t))
                                      (make-check-location 'loc))
                                fail)]
             [else
              (with-check-info*
               (list (make-check-name '|check-simplified?|)
                     (make-check-actual vT)
                     (make-check-expected vP)
                     (make-check-location 'loc))
               (λ () (check-equal? vT vP)))])))]
    [(_ term pattern)
     (syntax/loc stx (check-simplified? term pattern #:timeout #f))]))

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