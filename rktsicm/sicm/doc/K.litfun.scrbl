#lang scribble/manual

@(require (for-syntax racket/base)
          "helpers.rkt"
          (for-label sicm/kernel/litfun))

@title[#:tag "kernel/litfun"]{Litfun}
@defmodule[sicm/kernel/litfun #:packages ("rktsicm")]

@deftempproc*[
 *literal-reconstruction*
 ->
 Any
 DOWN
 DOWN*
 Hamiltonian
 Lagrangian
 Real
 UP
 UP*
 X
 X*
 ^
 all-satisfied
 default-function-type
 f:domain-types
 f:range-type
 length->exact-arity
 litderiv
 literal-apply
 literal-function
 literal-function?
 literal-partial
 litfun
 permissive-function-type
 starify
 type->arity
 type->domain
 type->domain-types
 type->range-type
 type-expression->predicate
 type-expression->type-tag
 typed-function]
