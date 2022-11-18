#lang racket/base

(require (for-syntax racket/base)
         rackunit
         racket/match
         racket/list
         syntax/srcloc
         racket/syntax
         (only-in "../kernel-gnrc.rkt" simplify)
         "../display/suppress-args.rkt")

(provide check-unique-match? check-simplified?
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
    [(_ term pattern)
     (with-syntax ([loc (list (syntax-source stx)
                              (syntax-line stx)
                              (syntax-column stx)
                              (syntax-position stx)
                              (syntax-span stx))])
       #`(let ([T (simplify (rename-expression (arg-suppressor+ (simplify term))))]
               [P (simplify (rename-expression (arg-suppressor+ (simplify pattern))))])
           (with-check-info*
            (list (make-check-name '|check-simplified?|)
                  (make-check-actual T)
                  (make-check-expected P)
                  (make-check-location 'loc))
            (λ () (check-equal? T P)))))]))

(module+ test
  (require "../main.rkt") ;; needed to use simplified
  (check-simplified? '(+ (* (sin x) (cos y)) (+ 4 5))
                    '(+ 9 (* (cos y) (sin x))))
  (check-exn exn:fail?
             (λ ()
               (check-simplified? '(+ 9 (* (sin x) (cos y)))
                                  '(+ 9 (* (sin y) (cos z)))))))