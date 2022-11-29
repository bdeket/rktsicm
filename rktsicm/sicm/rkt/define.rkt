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

(define-syntax-parameter default-object?
  (λ (stx) (raise-syntax-error 'default-object?
                               "only valid within lambda with #:optional "
                               stx)))

(define the-default-object (gensym 'default))

(define-syntax (mylambda stx)
  (syntax-parse stx
    [(_ (margs ... #:optional . (rst ... . e)) body ...)
     (with-syntax ([(oargs ...) (syntax->list #'(rst ...))])
       (quasisyntax/loc stx
         (lambda (margs ... [oargs the-default-object] ... . e)
           (syntax-parameterize ([default-object? (syntax-rules ()
                                                    [(_ a) (eq? the-default-object a)])])
             body ...))))]
    [(_ (margs ... . e) body ...)
     (let ()
       (when (member '#:optional (syntax->datum #'(margs ...)))
         (raise-syntax-error 'lambda "reserved #:optional keyword can not be used"))
       (syntax/loc stx (lambda (margs ... . e) body ...)))]))
(define-syntax (mydefine stx)
  (syntax-case stx ()
    [(_ (id . margs) body ...)
     (syntax/loc stx (define id (mylambda margs body ...)))]
    [(_ id expr)
     (syntax/loc stx (define id expr))]
    [(_ id)
     (syntax/loc stx (define id (gensym 'undefined)))]))

(define-syntax-rule (define-integrable head body ...)
  (begin-encourage-inline (mydefine head body ...)))

(module+ test
  (require rackunit)
  (mydefine (do1 a b #:optional c)
            (if (default-object? c)
                (list 'do1 a b)
                (list 'do1 a b c)))
  (check-equal? (do1 1 2) '(do1 1 2))
  (check-equal? (do1 1 2 3) '(do1 1 2 3))
  
  (mydefine (do2 a b)
            (list 'do2 a b))
  (check-equal? (do2 1 2) '(do2 1 2))
  
  (mydefine do3 'do3)
  (mydefine do3*)
  
  (mydefine (do4 a b #:optional c d)
            (list 'do4 a b c d))
  (check-equal? (do4 1 2) `(do4 1 2 ,the-default-object ,the-default-object))
  (check-equal? (do4 1 2 3) `(do4 1 2 3 ,the-default-object))
  (check-equal? (do4 1 2 3 4) '(do4 1 2 3 4))
  
  (mydefine (do5 a b #:optional c d . e)
            (list 'do5 a b c d e))
  (check-equal? (do5 1 2 3 4 5 6 7) '(do5 1 2 3 4 (5 6 7)))
  (check-equal? (do5 1 2) `(do5 1 2 ,the-default-object ,the-default-object ()))
  
  (mydefine (do6 a b . c)
            (list 'do6 a b c))
  (check-equal? (do6 1 2 3 4 5 6) `(do6 1 2 (3 4 5 6)))

  (mydefine (a #:optional b)
    (c b))
  (mydefine (c #:optional b)
    (if (default-object? b)
        'ok
        'nok))
  (check-equal? (a) 'ok)
  )