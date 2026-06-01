#lang scribble/manual

@(require (for-syntax racket/base)
          "helpers.rkt"
          (for-label sicm/kernel/iterat))

@title[#:tag "kernel/iterat"]{Iterat}
@defmodule[sicm/kernel/iterat #:packages ("rktsicm")]

@deftempproc*[
 array-by-cols
 array-by-rows
 array-copy
 array-elementwise
 array-ref
 array-set!
 array-with-substituted-col
 array-with-substituted-row
 generate-array
 generate-list
 generate-vector
 list-with-substituted-coord
 list:generate
 nth-col
 nth-row
 num-cols
 num-rows
 transpose-array
 vector-accumulate
 vector-elementwise
 vector-exists
 vector-forall
 vector-with-substituted-coord]
