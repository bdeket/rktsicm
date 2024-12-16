#lang racket/base

(provide (rename-out [the-undefined-value undefined-value]) undefined-value?)

(struct undefined-value ())
(define the-undefined-value (undefined-value))
