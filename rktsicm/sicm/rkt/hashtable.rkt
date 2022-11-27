#lang racket/base

(provide (all-defined-out))

(define hash-table/get hash-ref)
(define hash-table-ref/default hash-ref)
(define hash-table/put! hash-set!)
(define hash-table-set! hash-set!)
(define make-key-weak-eq-hash-table make-weak-hasheq)
(define make-eq-hash-table make-hasheq)
(define hash-table->alist hash->list)
(define hash-table? hash?)
