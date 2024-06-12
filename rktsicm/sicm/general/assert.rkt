#lang racket/base

(provide assert)

(require (for-syntax racket/base))

(define-syntax (assert stx)
  (syntax-case stx ()
    [(_ test) (quasisyntax/loc stx (unless test #,(syntax/loc stx (error (format "assertion failed: ~a" 'test)))))]
    [(_ test msg) (quasisyntax/loc stx (unless test #,(syntax/loc stx (error (format "~a:\n\tassertion failed: ~a"msg 'test)))))]
    [(_ test msg proc ...) (quasisyntax/loc stx (unless test #,(syntax/loc stx (error (format "~a: ~a" (list proc ...) msg)))))]))
