#lang racket/base

(provide (all-defined-out))
(require (for-syntax racket/base))

(define-syntax int:< (make-rename-transformer #'<))
(define-syntax int:<= (make-rename-transformer #'<=))
(define-syntax int:= (make-rename-transformer #'=))
(define-syntax int:> (make-rename-transformer #'>))
(define-syntax int:>= (make-rename-transformer #'>=))

(define-syntax int:+ (make-rename-transformer #'+))
(define-syntax int:- (make-rename-transformer #'-))
(define-syntax int:* (make-rename-transformer #'*))
(define-syntax int:quotient (make-rename-transformer #'quotient))

(define (int:zero? x) (eq? 0 x))
(define (int:negate x) (- x))

(define int:->flonum exact->inexact)

