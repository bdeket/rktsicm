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
         (submod racket/performance-hint begin-encourage-inline)
         (only-in racket/list take))

(define true #t)
(define false #f)
(define (delete itm lst) (remove* (list itm) lst))
(define (delq itm lst) (remq* (list itm) lst))
(define every andmap) ;; not definedin mitscheme, nor scmutils...
(define any ormap)
(define iota (case-lambda [(e) (for/list ([i (in-range e)]) i)]
                          [(e s)(for/list ([i (in-range s e 1)]) i)]
                          [(e s t)(for/list ([i (in-range s e t)]) i)]))
(define (there-exists? l p?) (ormap p? l))
(define cons* list*)
(define make-initialized-list build-list)
(define list-head take)
(define generate-uninterned-symbol gensym)
(define find findf)
(define make-initialized-vector build-vector)
(define string:<? string<?)
(define-syntax-rule (define-integrable head body ...)
  (begin-encourage-inline (define head body ...)))