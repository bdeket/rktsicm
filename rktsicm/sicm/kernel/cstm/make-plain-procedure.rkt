#lang racket/base

(require (only-in racket/syntax format-id)
         (only-in racket/list last take)
         "arity.rkt")

(provide make-plain-procedure)

(define (make-plain-procedure f A)
  (define arity (if (procedure-arity? A)
                    (normalize-arity A)
                    (raise-argument-error 'make-generic-operator "procedure-arity?" arity)))
  (eval-syntax
          (cond
           [(exactly-n? arity)
            (with-syntax ([(x ...) (build-list arity
                                                     (λ (i) (format-id #f "x~a" i)))])
              #`(λ (x ...)
                  (#%plain-app #,f x ...)))]
           [(arity-at-least? arity)
            (with-syntax ([(x ...) (build-list (arity-at-least-value arity)
                                                     (λ (i) (format-id #f "x~a" i)))])
              #`(λ (x ... . y)
                (#%plain-app apply #,f x ... y)))]
           [else
            (define END (last arity))
            (define LEN (length arity))
            (cond
              [(number? END)
               (with-syntax ([((Xs ...)...)
                              (map (λ (x) (build-list x (λ (i) (format-id #f "x~a" i))))
                                   arity)])
                 #`(case-lambda [(Xs ...) (#%plain-app #,f Xs ...)]...))]
              [else
               (with-syntax ([((Xs ...)...)
                              (map (λ (x) (build-list x (λ (i) (format-id #f "x~a" i))))
                                   (take arity (- LEN 1)))]
                             [(Ys ...) (build-list (arity-at-least-value END)
                                                        (λ (i) (format-id #f "x~a" i)))])
                 #`(case-lambda [(Xs ...) (#%plain-app #,f Xs ...)]...
                                [(Ys ... . y) (#%plain-app apply #,f Ys ... y)]))])])))