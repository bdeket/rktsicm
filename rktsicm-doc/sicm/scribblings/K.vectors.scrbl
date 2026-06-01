#lang scribble/manual

@(require (for-syntax racket/base)
          "helpers.rkt"
          (for-label sicm/kernel/vectors))

@title[#:tag "kernel/vectors"]{Vectors}
@defmodule[sicm/kernel/vectors #:packages ("rktsicm")]

@deftempproc*[
 abstract-vector
 av:arity
 av:zero-like
 complex-norm
 euclidean-norm
 general-inner-product
 literal-vector
 make-vector-combination
 maxnorm
 scalar*vector
 v:apply
 v:arity
 v:basis-unit?
 v:conjugate
 v:cross-product
 v:cube
 v:dimension
 v:dot-product
 v:elementwise
 v:generate
 v:inexact?
 v:inner-product
 v:make-basis-unit
 v:make-unit
 v:make-zero
 v:negate
 v:partial-derivative
 v:scale
 v:square
 v:type
 v:type-predicate
 v:unit?
 v:zero-like
 v:zero?
 vector*scalar
 vector+vector
 vector-vector
 vector/scalar
 vector:elementwise
 vector:generate
 vector=vector
 vectors:assign-operations]
