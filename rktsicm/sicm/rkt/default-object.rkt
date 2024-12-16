#lang racket/base

(provide (rename-out [the-default-object default-object]) default-object?)

(struct default-object ())
(define the-default-object (default-object))