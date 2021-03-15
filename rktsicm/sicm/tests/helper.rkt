#lang racket/base

(require (for-syntax racket/base)
         rackunit
         racket/match
         racket/list
         syntax/srcloc)

(provide check-unique-match?)

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