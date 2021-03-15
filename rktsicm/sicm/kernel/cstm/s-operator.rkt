#lang racket/base

(provide (all-defined-out))

(require "../../rkt/applyhook.rkt"
         "../../general/assert.rkt"
         "types.rkt")

(define (make-operator p [name #f] [subtype #f] [arity (procedure-arity p)] . opts)
  (make-apply-hook p `(,operator-type-tag ,subtype ,name ,arity ,@opts)))

(define (operator? x)
  (and (apply-hook? x)
       (eq? (car (apply-hook-extra x))
	    operator-type-tag)))

(define (operator-procedure op)
  (assert (operator? op))
  (apply-hook-procedure op))

(define (operator-subtype op)
  (assert (operator? op))
  (cadr (apply-hook-extra op)))

(define (operator-name op)
  (assert (operator? op))
  (caddr (apply-hook-extra op)))

(define (operator-arity op)
  (assert (operator? op))
  (cadddr (apply-hook-extra op)))

(define (operator-optionals op)
  (assert (operator? op))
  (cddddr (apply-hook-extra op)))
