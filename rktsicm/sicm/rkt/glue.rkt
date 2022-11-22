#lang racket/base

(provide (all-from-out
          "applyhook.rkt"
          "default-object.rkt"
          "fixnum.rkt"
          "if.rkt"
          "int.rkt"
          "racket-help.rkt"
          "undefined.rkt")
         (all-defined-out))

(require "applyhook.rkt"
         "default-object.rkt"
         "fixnum.rkt"
         "if.rkt"
         "int.rkt"
         "racket-help.rkt"
         "undefined.rkt")

(define true #t)
(define false #f)