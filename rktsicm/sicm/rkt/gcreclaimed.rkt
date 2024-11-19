#lang racket/base

(provide (all-defined-out))

(define gc-reclaimed-object (gensym))
(define (gc-reclaimed-object? v) (eq? gc-reclaimed-object v))
