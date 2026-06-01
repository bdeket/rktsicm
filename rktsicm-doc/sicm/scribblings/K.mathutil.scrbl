#lang scribble/manual

@(require (for-syntax racket/base)
          "helpers.rkt"
          (for-label sicm/kernel/mathutil))

@title[#:tag "kernel/mathutil"]{Mathutil}
@defmodule[sicm/kernel/mathutil #:packages ("rktsicm")]

@deftempproc*[
 adjust-end
 adjust-index
 component
 g:arg-scale
 g:arg-shift
 g:compose
 g:compose-2
 g:compose-bin
 g:cube
 g:ref
 g:size
 ratnum?
 ref-internal]
