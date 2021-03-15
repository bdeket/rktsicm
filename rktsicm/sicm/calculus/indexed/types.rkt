#lang racket/base

(provide (all-defined-out))

(require racket/fixnum
         "../../general/assert.rkt"
         "../../general/eq-properties.rkt"
         )

;;;;    Minimal support for Indexed Objects
;;; e.g. the components of tensors relative to a basis.

;;; A minimal interface for multi-index stuff.

(define (argument-types proc)
  (eq-get proc 'argument-types))

(define has-argument-types? argument-types)

(define (declare-argument-types! proc argument-types)
  (assert (procedure? proc))
  (eq-put! proc 'argument-types argument-types))

;;; argument-types are, for example 
;;;    (list 1form-field? vector-field? vector-field?), 
;;; for a Christoffel-2: it takes one 1form field and two vector fields.

(define (index-types proc)
  (eq-get proc 'index-types))

(define has-index-types? index-types)

(define (declare-index-types! proc index-types)
  (assert (procedure? proc))
  (eq-put! proc 'index-types index-types))
