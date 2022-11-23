#lang racket/base

(provide (rename-out [mydefine define])
         default-object?
         define-integrable)

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     racket/stxparam)
         racket/stxparam
         (submod racket/performance-hint begin-encourage-inline))

(define-syntax-parameter default-object?
  (λ (stx) (raise-syntax-error 'default-object?
                               "only valid within define with #:optional "
                               stx)))
(define-syntax (mydefine stx)
  (syntax-parse stx
    [(_ (id margs ... #:optional . (rst ... . e)) body ...)
     (with-syntax ([(oargs ...) (syntax->list #'(rst ...))])
       (quasisyntax/loc stx
         (define id
           (let ([default-object (gensym)])
             (λ (margs ... [oargs default-object] ... . e)
               (syntax-parameterize ([default-object? (syntax-rules ()
                                                        [(_ a) (eq? default-object a)])])
                 body ...))))))]
    [(_ (id margs ...) body ...)
     (let ()
       (when (member '#:optional (syntax->datum #'(margs ...)))
         (raise-syntax-error 'define "reserved #:optional keyword can not be used"))
       (syntax/loc stx (define (id margs ...) body ...)))]
    [(_ id expr)
     (syntax/loc stx (define id expr))]))

(define-syntax-rule (define-integrable head body ...)
  (begin-encourage-inline (mydefine head body ...)))

(module+ test
  (mydefine (do1 a b #:optional c)
            (if (default-object? c)
                (list 'do1 a b)
                (list 'do1 a b c)))
  (mydefine (do2 a b)
            (list 'do2 a b))
  (mydefine do3 'do3)
  (mydefine (do4 a b #:optional c d)
            (list 'do4 a b c d))
  (mydefine (do5 a b #:optional c d . e)
            (list 'do5 a b c d e))
  (mydefine (do6 a b . c)
            (list 'do6 a b c))
  (list (do1 1 2)
        (do1 1 2 3)
        (do2 1 2)
        (do4 1 2)
        (do4 1 2 3)
        (do4 1 2 3 4)
        (do5 1 2 3 4 5 6 7)
        (do5 1 2)
        (do6 1 2 3 4 5 6)))