#lang scribble/manual

@(require (for-syntax racket/base)
          "helpers.rkt"
          (for-label sicm/kernel/operator))

@title[#:tag "kernel/operator"]{Operator}
@defmodule[sicm/kernel/operator #:packages ("rktsicm")]

@deftempproc*[
 expn
 make-op
 make-operator
 o:*
 o:+
 o:-
 o:arity
 o:cos
 o:exp
 o:expt
 o:f*o
 o:f+o
 o:f-o
 o:identity
 o:negate
 o:o*f
 o:o+f
 o:o-f
 o:o/n
 o:one-like
 o:sin
 o:type
 o:type-predicate
 o:zero-like
 operator-arity
 operator-merge-arities
 operator-merge-optionals
 operator-merge-subtypes
 operator-name
 operator-optionals
 operator-procedure
 operator-subtype
 operator:assign-operations
 set-operator-optionals!
 simple-operator?]
