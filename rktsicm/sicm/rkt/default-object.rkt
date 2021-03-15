#lang racket/base

(provide default-object default-object?)

(define default-object (gensym 'default))
(define (default-object? x) (eq? x default-object))