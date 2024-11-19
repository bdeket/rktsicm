#lang scribble/manual

@(require (for-syntax racket/base)
          "helpers.rkt"
          (for-label sicm/kernel/diff
		             sicm/kernel/deriv))

@title[#:tag "kernel/diff"]{Diff}
@defmodule[sicm/kernel/diff #:packages ("rktsicm")]

@deftempproc*[
 *active-tags*
 <differential-tags?
 <dt
 <dts
 =dt
 d:*
 d:+
 diff:*
 diff:+
 diff:-
 diff:/
 diff:abs
 diff:acos
 diff:apply
 diff:arity
 diff:asin
 diff:assign-operations
 diff:atan1
 diff:atan2
 diff:binary-comparator
 diff:binary-op
 diff:cos
 diff:cosh
 diff:derivative
 diff:exp
 diff:expt
 diff:identity
 diff:invert
 diff:log
 diff:negate
 diff:one-like
 diff:one?
 diff:power
 diff:sin
 diff:sinh
 diff:sqrt
 diff:square
 diff:type
 diff:type-predicate
 diff:unary-op
 diff:zero-like
 diff:zero?
 differential->terms
 differential-coefficient
 differential-object
 differential-of
 differential-tag-count
 differential-tags
 differential-term-list
 dtl:*
 dtl:+
 extract-dx-differential
 extract-dx-function
 extract-dx-operator
 extract-dx-part
 finite-part
 infinitesimal-part
 insert-differential-tag
 intersect-differential-tags
 make-differential-quantity
 make-differential-tag
 make-differential-term
 make-x+dx
 max-order-tag
 remove-differential-tag
 replace-differential-tag
 replace-dx-function
 replace-dx-operator
 same-differential-tags?
 simple-derivative-internal
 tag-active?
 tdtl:*
 terms->differential
 terms->differential-collapse
 union-differential-tags
 with-active-tag
 with-tag
 without-tag]

@section[#:tag "kernel/deriv"]{Deriv}
@defmodule[sicm/kernel/deriv #:packages ("rktsicm")]

@deftempproc*[
 deriv:assign-operations
 deriv:euclidean-structure
 deriv:multivariate-derivative]
