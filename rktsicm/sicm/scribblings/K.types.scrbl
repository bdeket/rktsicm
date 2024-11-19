#lang scribble/manual

@(require (for-syntax racket/base)
          "helpers.rkt"
          (for-label sicm/kernel/types))

@title[#:tag "kernel/types"]{Types}
@defmodule[sicm/kernel/types #:packages ("rktsicm")]

@deftempproc*[
 *down*
 *function*
 *matrix*
 *number*
 *up*
 *vector*
 abstract-down-type-tag
 abstract-down?
 abstract-function-type-tag
 abstract-function?
 abstract-matrix-type-tag
 abstract-matrix?
 abstract-number?
 abstract-predicate
 abstract-quantity?
 abstract-structure?
 abstract-type-tag
 abstract-type-tags
 abstract-up-type-tag
 abstract-up?
 abstract-vector-type-tag
 abstract-vector?
 add-to-numerical-quantity?
 cofunction?
 compound-type-tag?
 compound-type-tags
 concrete-predicate
 differential-type-tag
 differential?
 down-quantity?
 down-type-tag
 down?
 f:expression
 function-quantity?
 function-type-tag
 function?
 literal-number?
 literal-real?
 make-type
 matrix-quantity?
 matrix-type-tag
 matrix?
 not-compound?
 not-d-c-u?
 not-differential-or-compound?
 not-differential?
 not-operator?
 not-series?
 number-type-tag
 numerical-quantity?
 operator-type-tag
 operator?
 quantity-predicate
 quaternion-quantity?
 quaternion-type-tag
 quaternion?
 scalar?
 series-type-tag
 series?
 square-abstract-matrix?
 square-matrix?
 structure?
 type-tag
 type-tags
 typed-function?
 typed-or-abstract-function?
 unit-type-tag
 units?
 up-quantity?
 up-type-tag
 up?
 vector-quantity?
 vector-type-tag
 with-units-type-tag
 with-units?]
