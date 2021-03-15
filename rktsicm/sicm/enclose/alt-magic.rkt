#lang racket/base

(provide (all-defined-out))

(require "../rkt/environment.rkt")

;; replacing things from enclose and magic...
;; that I don't understand/know how to replicate in racket

;the original in magic.sch compiles the procedure for faster excecution
;i don't know if that is an actual option in racket
(define (compile-procedure proc)
  (unless (procedure? proc)
    (raise-argument-error 'compiled-procedure "procedure" proc))
  proc)

(define (lambda->numerical-procedure sexp)
  (eval sexp numerical-environment))
