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
         "undefined.rkt")

(define true #t)
(define false #f)
(define delete remove)
(define every andmap)