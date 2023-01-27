#lang racket/base

(provide (rename-out [mydefine define][mylambda lambda])
         default-object?
         define-integrable)

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     racket/stxparam)
         racket/stxparam
         (submod racket/performance-hint begin-encourage-inline))

#;(define-syntax-parameter default-object?
  (λ (stx) (raise-syntax-error 'default-object?
                               "only valid within lambda with #:optional "
                               stx)))
(define the-default-object (gensym 'default))
(define (default-object? x) (eq? the-default-object x))

(define-syntax (mylambda stx)
  (syntax-parse stx
    [(_ (margs ... #:optional . (rst ... . e)) body ...)
     (with-syntax ([(oargs ...) (syntax->list #'(rst ...))])
       (quasisyntax/loc stx
         (lambda (margs ... [oargs the-default-object] ... . e)
           #;(syntax-parameterize ([default-object? (syntax-rules ()
                                                    [(_ a) (eq? the-default-object a)])])
             body ...)
           body ...)))]
    [(_ (margs ... . e) body ...)
     (let ()
       (when (member '#:optional (syntax->datum #'(margs ...)))
         (raise-syntax-error 'lambda "reserved #:optional keyword can not be used"))
       (syntax/loc stx (lambda (margs ... . e) body ...)))]))
(define-syntax (mydefine stx)
  (syntax-case stx ()
    [(_ (id . margs) body ...)
     (if (list? (syntax-e #'id))
         (quasisyntax/loc stx (mydefine id #,(quasisyntax/loc stx (mylambda margs body ...))))
         (quasisyntax/loc stx (define id #,(quasisyntax/loc stx (mylambda margs body ...)))))]
    [(_ id expr)
     (syntax/loc stx (define id expr))]
    [(_ id)
     (syntax/loc stx (define id (gensym 'undefined)))]))

(define-syntax-rule (define-integrable head body ...)
  (begin-encourage-inline (mydefine head body ...)))
