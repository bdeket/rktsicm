#lang scribble/manual

@(require (for-syntax racket/base)
          "helpers.rkt"
          (for-label sicm/kernel/utils))

@title[#:tag "kernel/utils"]{Utils}
@defmodule[sicm/kernel/utils #:packages ("rktsicm")]

@deftempproc*[
 a-reduce
 accumulation
 all-equal?
 alphaless?
 any?
 apply-to-all
 binary-combine
 bracket
 clear-notes!
 compose-2
 compose-bin
 compose-n
 concatenate-names
 concatenate-names-maker
 constant
 cpp
 defer-application
 display-note
 do-down
 do-up
 identity
 inverse-accumulation
 iterate-until-stable
 iterated
 left-circular-shift
 make-function-of-arguments
 make-function-of-vector
 make-map
 make-pairwise-test
 nary-combine
 none?
 note-that!
 pp-it
 print-breadth
 print-depth
 right-circular-shift
 show-notes
 sign
 the-null-symbol
 unary-combine
 wallp-pp
 watch-it]

@section[#:tag "kernel/utils/arity"]{Arity}
@deftype*[arity? [*exactly-zero* *at-least-zero*
                  *exactly-one* *at-least-one*
                  *exactly-two* *at-least-two*
                  *one-or-two*
                  *exactly-three* *at-least-three*]]
Fixed definitions of procedure arities
@deftempproc*[
 any-number?
 arity-includes?
 arity-intersect
 arity-min
 combine-arity
 exact-arity
 exactly-n?
 joint-arity
 normalize-arity]
