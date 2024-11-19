#lang scribble/manual

@(require (for-syntax racket/base)
          "helpers.rkt"
          (for-label sicm/kernel/modarith))

@title[#:tag "kernel/modarith"]{Modarith}
@defmodule[sicm/kernel/modarith #:packages ("rktsicm")]

@deftempproc*[
 mod:*
 mod:+
 mod:-
 mod:/
 mod:=
 mod:binary-combine
 mod:chinese-remainder
 mod:expt
 mod:invert
 mod:make
 mod:make-internal
 mod:modulus
 mod:reduce
 mod:residue
 mod:unary-combine
 modarith:assign-operations
 modint:*
 modint:+
 modint:-
 modint:/
 modint:chinese-remainder
 modint:expt
 modint:invert
 modint?
 modular-type-tag]
