#lang scribble/manual

@(require (for-syntax racket/base)
          "helpers.rkt"
          (for-label sicm/kernel/function))

@title[#:tag "kernel/function"]{Function}
@defmodule[sicm/kernel/function #:packages ("rktsicm")]

@deftempproc*[
 coerce-to-function
 f:arity
 f:binary
 f:identity-like
 f:one-like
 f:transpose
 f:type
 f:type-predicate
 f:unary
 f:zero-like
 function:assign-operations
 p-rename]
