#lang racket/base

(provide (all-defined-out))
(struct apply-hook (procedure [extra #:mutable])
  #:property prop:procedure (λ (this . args) (apply (apply-hook-procedure this) args))
  #:extra-constructor-name make-apply-hook
  #:property prop:object-name (λ (ah) (object-name (apply-hook-procedure ah))))