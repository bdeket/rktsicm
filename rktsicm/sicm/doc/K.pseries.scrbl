#lang scribble/manual

@(require (for-syntax racket/base)
          "helpers.rkt"
          (for-label sicm/kernel/pseries))

@title[#:tag "kernel/pseries"]{Pseries}
@defmodule[sicm/kernel/pseries #:packages ("rktsicm")]

@deftempproc*[
 *integrate-series
 add-series
 atan-series
 binomial-series
 coefficient*series
 coefficient+series
 coefficient-series
 coefficient/series
 constant-series
 cos-series
 cosh-series
 div$series
 exp-series
 integral-series-tail
 integrate-helper
 invert-series
 make-series
 mul$series
 mul-series
 negate-stream
 partial-sums
 partial-sums-stream
 power-series
 pseries:assign-operations
 series
 series*coefficient
 series+coefficient
 series->stream
 series-coefficient
 series-wrapper
 series/coefficient
 series:->function
 series:add
 series:arity
 series:derivative
 series:div
 series:elementwise
 series:expt
 series:for-each
 series:function->
 series:generate
 series:identity
 series:inflate
 series:invert
 series:mul
 series:negate
 series:one
 series:one-like
 series:print
 series:promote-arity
 series:ref
 series:same-arity
 series:sub
 series:sum
 series:type
 series:type-predicate
 series:value
 series:zero
 series:zero-like
 sin-series
 sinh-series
 stream:c*s
 stream:s/c
 sub-series
 tan-series]
