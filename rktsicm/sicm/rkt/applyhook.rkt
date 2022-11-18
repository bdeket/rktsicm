#lang racket/base

(provide (all-defined-out))
(struct apply-hook (procedure [extra #:mutable])
  #:property prop:procedure 0
  #:extra-constructor-name make-apply-hook
  #:property prop:object-name (λ (ah) (object-name (apply-hook-procedure ah))))