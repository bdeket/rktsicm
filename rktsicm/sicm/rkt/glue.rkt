#lang racket/base

(provide (all-from-out
          "applyhook.rkt"
          "hashtable.rkt"
          "1d-table.rkt"
          "default-object.rkt"
          "fixnum.rkt"
          "if.rkt"
          "int.rkt"
          "racket-help.rkt"
          "undefined.rkt")
         (all-defined-out))

(require "applyhook.rkt"
         "hashtable.rkt"
          "1d-table.rkt"
         "default-object.rkt"
         "fixnum.rkt"
         "if.rkt"
         "int.rkt"
         "racket-help.rkt"
         "undefined.rkt"
         (submod racket/performance-hint begin-encourage-inline))

(define true #t)
(define false #f)
(define delete remove*)
(define delq remq*)
(define every andmap) ;; not definedin mitscheme, nor scmutils...
(define (there-exists? l p?) (ormap p? l))
(define cons* list*)
(define-syntax-rule (define-integrable head body ...)
  (begin-encourage-inline (define head body ...)))