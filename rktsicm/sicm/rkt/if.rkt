#lang racket/base

(provide if)

(require (for-syntax racket/base)
         (rename-in racket/base [if rkt:if]))

(define-syntax (if stx)
  (syntax-case stx ()
    [(if test? yes no) (syntax/loc stx (rkt:if test? yes no))]
    [(if test? yes)    (syntax/loc stx (when test? yes))]))
