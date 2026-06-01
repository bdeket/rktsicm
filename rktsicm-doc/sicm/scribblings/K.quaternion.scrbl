#lang scribble/manual

@(require (for-syntax racket/base)
          "helpers.rkt"
          (for-label sicm/kernel/quaternion))

@title[#:tag "kernel/quaternion"]{Quaternion}
@defmodule[sicm/kernel/quaternion #:packages ("rktsicm")]

@deftempproc*[
 4x4->quaternion
 angle-axis->quaternion
 make-quaternion
 q:*
 q:+
 q:-
 q:->4x4
 q:->angle-axis
 q:->rotation-matrix
 q:->vector
 q:/
 q:1
 q:3vector
 q:4x4->
 q:=
 q:angle-axis->
 q:apply
 q:arity
 q:conjugate
 q:exp
 q:i
 q:inexact?
 q:invert
 q:j
 q:k
 q:log
 q:magnitude
 q:make
 q:make-unit
 q:negate
 q:partial-derivative
 q:real&3vector->
 q:real-part
 q:ref
 q:rotate
 q:rotation-matrix->
 q:type
 q:type-predicate
 q:unit?
 q:zero-like
 q:zero?
 quaternion
 quaternion*quaternion
 quaternion*scalar
 quaternion+quaternion
 quaternion->3vector
 quaternion->4x4
 quaternion->angle-axis
 quaternion->real-part
 quaternion->rotation-matrix
 quaternion->vector
 quaternion-quaternion
 quaternion-ref
 quaternion/quaternion
 quaternion/scalar
 quaternion:assign-operations
 real&3vector->quaternion
 rotation-matrix->quaternion
 s:1
 s:i
 s:j
 s:k
 scalar*quaternion]
