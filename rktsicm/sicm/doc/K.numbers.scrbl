#lang scribble/manual

@(require (for-syntax racket/base)
          "helpers.rkt"
          (for-label sicm/kernel/numbers))

@title[#:tag "kernel/numbers"]{Numbers}
@defmodule[sicm/kernel/numbers #:packages ("rktsicm")]

@deftempproc*[
 *known-reals*
 *numbers-are-constant-functions*
 abn:=
 abn:one?
 abn:zero?
 an:=
 an:one-like
 an:one?
 an:zero-like
 an:zero?
 declare-known-reals
 declare-unknown-reals
 known-real?
 literal-number
 make-numerical-combination
 n:*
 n:+
 n:-
 n:/
 n:<
 n:<=
 n:=
 n:>
 n:>=
 n:abs
 n:acos
 n:angle
 n:arity
 n:asin
 n:atan
 n:conjugate
 n:cos
 n:cosh
 n:csc
 n:csch
 n:cube
 n:deriv
 n:exp
 n:exp10
 n:exp2
 n:expt
 n:gcd
 n:imag-part
 n:inexact?
 n:invert
 n:log
 n:log10
 n:log2
 n:magnitude
 n:make-polar
 n:make-rectangular
 n:negate
 n:one-like
 n:one?
 n:real-part
 n:revdivide
 n:sec
 n:sech
 n:self
 n:sigma
 n:sin
 n:sinh
 n:sqrt
 n:square
 n:tan
 n:tanh
 n:type
 n:type-predicate
 n:zero-like
 n:zero?
 numbers:assign-operations
 with-known-reals]
