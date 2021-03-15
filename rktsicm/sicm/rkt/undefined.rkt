#lang racket/base

(provide undefined-value undefined-value?)

(define undefined-value (gensym 'undefined))
(define (undefined-value? x) (eq? undefined-value x))
