#lang racket/base

(provide (all-defined-out))

(struct 1d-table (v))
(define (make-1d-table) (1d-table (make-weak-hash)))
(define (1d-table/get T k nf) (hash-ref (1d-table-v T) k nf))
(define (1d-table/put! T k v) (hash-set! (1d-table-v T) k v))